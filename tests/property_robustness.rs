//! Seeded robustness tests for parser/typechecker/verifier determinism.
//!
//! This suite is evaluation-facing rather than part of the default fast test
//! path. It uses a fixed seed corpus and a restricted generator so the results
//! are reproducible and suitable for dissertation evaluation.

use std::sync::Mutex;
use veritas::pipeline::{compile, compile_verbose};
use veritas::verifier::{verify_dtal, verify_dtal_text};

static ROBUSTNESS_LOCK: Mutex<()> = Mutex::new(());

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Family {
    ValidArithmetic,
    ValidArrays,
    InvalidType,
    InvalidSyntax,
}

#[derive(Clone, Copy, Debug)]
struct CorpusCase {
    family: Family,
    seed: u64,
}

#[derive(Clone, Copy, Debug)]
struct SeededRng {
    state: u64,
}

impl SeededRng {
    fn new(seed: u64) -> Self {
        Self {
            state: seed ^ 0x9E37_79B9_7F4A_7C15,
        }
    }

    fn next_u32(&mut self) -> u32 {
        self.state = self
            .state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (self.state >> 32) as u32
    }

    fn next_bool(&mut self) -> bool {
        self.next_u32() & 1 == 0
    }

    fn choose(&mut self, upper_exclusive: usize) -> usize {
        (self.next_u32() as usize) % upper_exclusive
    }

    fn int_inclusive(&mut self, low: i32, high: i32) -> i32 {
        let span = (high - low + 1) as u32;
        low + (self.next_u32() % span) as i32
    }

    fn nonzero_int(&mut self, low: i32, high: i32) -> i32 {
        loop {
            let value = self.int_inclusive(low, high);
            if value != 0 {
                return value;
            }
        }
    }
}

fn load_corpus() -> Vec<CorpusCase> {
    include_str!("../eval/robustness_corpus.tsv")
        .lines()
        .skip(1)
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let mut cols = line.split('\t');
            let family = match cols.next().expect("family column") {
                "valid_arithmetic" => Family::ValidArithmetic,
                "valid_arrays" => Family::ValidArrays,
                "invalid_type" => Family::InvalidType,
                "invalid_syntax" => Family::InvalidSyntax,
                other => panic!("unexpected robustness family: {other}"),
            };
            let seed = cols
                .next()
                .expect("seed column")
                .parse::<u64>()
                .expect("seed should parse");
            CorpusCase { family, seed }
        })
        .collect()
}

fn gen_atom(rng: &mut SeededRng, vars: &[&str]) -> String {
    if rng.next_bool() {
        vars[rng.choose(vars.len())].to_string()
    } else {
        rng.int_inclusive(0, 6).to_string()
    }
}

fn gen_int_expr(rng: &mut SeededRng, depth: usize, vars: &[&str]) -> String {
    if depth == 0 || rng.next_bool() {
        return gen_atom(rng, vars);
    }

    let left = gen_int_expr(rng, depth - 1, vars);
    let right = match rng.choose(3) {
        0 => gen_int_expr(rng, depth - 1, vars),
        1 => rng.nonzero_int(1, 4).to_string(),
        _ => gen_atom(rng, vars),
    };
    let op = match rng.choose(2) {
        0 => "+",
        _ => "-",
    };
    format!("({left} {op} {right})")
}

fn generate_valid_arithmetic(seed: u64) -> String {
    let mut rng = SeededRng::new(seed);
    let init_x = rng.int_inclusive(0, 6);
    let init_y = rng.int_inclusive(0, 6);
    let helper_add = rng.nonzero_int(1, 3);
    let helper_expr = gen_int_expr(&mut rng, 2, &["a", "b"]);
    let body_expr_1 = gen_int_expr(&mut rng, 2, &["x", "y", "z"]);
    let body_expr_2 = gen_int_expr(&mut rng, 2, &["x", "y", "z", "w"]);

    format!(
        "fn helper(a: int, b: int) -> int {{
    let t: int = {helper_expr};
    t + {helper_add}
}}

fn main() -> int {{
    let x: int = {init_x};
    let y: int = {init_y};
    let z: int = helper(x, y);
    let w: int = {body_expr_1};
    {body_expr_2}
}}"
    )
}

fn generate_valid_arrays(seed: u64) -> String {
    let mut rng = SeededRng::new(seed);
    let a0 = rng.int_inclusive(0, 20);
    let a1 = rng.int_inclusive(0, 20);
    let bump = rng.nonzero_int(1, 6);
    let idx = rng.choose(4);

    format!(
        "fn main() -> int {{
    let mut arr: [int; 4] = [0; 4];
    arr[0] = {a0};
    arr[1] = {a1};
    arr[2] = arr[0] + arr[1];
    arr[3] = arr[2] + {bump};
    arr[{idx}]
}}"
    )
}

fn generate_invalid_type(seed: u64) -> String {
    let mut rng = SeededRng::new(seed);
    match rng.choose(4) {
        0 => {
            let lit = rng.int_inclusive(0, 9);
            format!(
                "fn main() -> int {{
    let x: int = true;
    x + {lit}
}}"
            )
        }
        1 => {
            let lit = rng.int_inclusive(0, 9);
            format!(
                "fn main() -> int {{
    if {lit} {{
        1
    }} else {{
        2
    }}
}}"
            )
        }
        2 => format!(
            "fn main() -> int {{
    let mut arr: [int; 4] = [0; 4];
    arr[0] = 1;
    arr[true]
}}"
        ),
        _ => {
            let lit = rng.int_inclusive(0, 9);
            format!(
                "fn inc(x: int) -> int {{
    x + 1
}}

fn main() -> int {{
    inc(false) + {lit}
}}"
            )
        }
    }
}

fn generate_invalid_syntax(seed: u64) -> String {
    let mut rng = SeededRng::new(seed);
    match rng.choose(4) {
        0 => "fn main() -> int { let x: int = 1 x }".to_string(),
        1 => "fn main() -> int { let x: int = (1 + ); x }".to_string(),
        2 => "fn main() -> int { let x: int = 1; ".to_string(),
        _ => {
            let lit = rng.int_inclusive(0, 9);
            format!("fn main() -> int {{ let x: int = ({lit} + 1; x }}")
        }
    }
}

fn generate_program(case: CorpusCase) -> String {
    match case.family {
        Family::ValidArithmetic => generate_valid_arithmetic(case.seed),
        Family::ValidArrays => generate_valid_arrays(case.seed),
        Family::InvalidType => generate_invalid_type(case.seed),
        Family::InvalidSyntax => generate_invalid_syntax(case.seed),
    }
}

fn label(case: CorpusCase) -> String {
    let family = match case.family {
        Family::ValidArithmetic => "valid_arithmetic",
        Family::ValidArrays => "valid_arrays",
        Family::InvalidType => "invalid_type",
        Family::InvalidSyntax => "invalid_syntax",
    };
    format!("{family}:seed={}", case.seed)
}

#[test]
#[ignore = "robustness evaluation suite; run explicitly"]
fn seeded_valid_programs_are_deterministic_and_verify() {
    let _guard = ROBUSTNESS_LOCK.lock().unwrap();

    for case in load_corpus() {
        if !matches!(case.family, Family::ValidArithmetic | Family::ValidArrays) {
            continue;
        }

        let source = generate_program(case);
        let first = compile_verbose(&source)
            .unwrap_or_else(|err| panic!("{}: unexpected compile failure: {}", label(case), err));
        let second = compile_verbose(&source)
            .unwrap_or_else(|err| panic!("{}: unexpected compile failure: {}", label(case), err));

        assert_eq!(
            first.dtal,
            second.dtal,
            "{}: emitted DTAL changed across repeated compilation",
            label(case)
        );
        assert_eq!(
            first.tokens,
            second.tokens,
            "{}: token stream changed across repeated compilation",
            label(case)
        );

        verify_dtal(&first.dtal_program).unwrap_or_else(|err| {
            panic!(
                "{}: in-memory DTAL verification failed: {}",
                label(case),
                err
            )
        });
        verify_dtal_text(&first.dtal).unwrap_or_else(|err| {
            panic!("{}: DTAL text verification failed: {:?}", label(case), err)
        });
    }
}

#[test]
#[ignore = "robustness evaluation suite; run explicitly"]
fn seeded_invalid_programs_fail_deterministically() {
    let _guard = ROBUSTNESS_LOCK.lock().unwrap();

    for case in load_corpus() {
        if !matches!(case.family, Family::InvalidType | Family::InvalidSyntax) {
            continue;
        }

        let source = generate_program(case);
        let first = compile(&source)
            .err()
            .unwrap_or_else(|| panic!("{}: expected compile failure but succeeded", label(case)))
            .to_string();
        let second = compile(&source)
            .err()
            .unwrap_or_else(|| panic!("{}: expected compile failure but succeeded", label(case)))
            .to_string();

        assert_eq!(
            first,
            second,
            "{}: compile diagnostics changed across repeated compilation",
            label(case)
        );
    }
}

#[test]
#[ignore = "robustness evaluation suite; run explicitly"]
fn seeded_corpus_classification_is_stable() {
    let _guard = ROBUSTNESS_LOCK.lock().unwrap();

    for case in load_corpus() {
        let source = generate_program(case);
        let first = compile(&source);
        let second = compile(&source);

        match (first, second) {
            (Ok(first_ok), Ok(second_ok)) => {
                assert_eq!(
                    first_ok.dtal,
                    second_ok.dtal,
                    "{}: success classification was stable but DTAL output changed",
                    label(case)
                );
            }
            (Err(first_err), Err(second_err)) => {
                assert_eq!(
                    first_err.to_string(),
                    second_err.to_string(),
                    "{}: failure classification was stable but diagnostics changed",
                    label(case)
                );
            }
            (Ok(_), Err(err)) => {
                panic!(
                    "{}: compilation flipped from success to failure: {}",
                    label(case),
                    err
                );
            }
            (Err(err), Ok(_)) => {
                panic!(
                    "{}: compilation flipped from failure to success: {}",
                    label(case),
                    err
                );
            }
        }
    }
}
