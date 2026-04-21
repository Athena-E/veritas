const N: usize = 64;
const TSTEPS: usize = 20;

fn init_array(a: &mut [[i64; N]; N], b: &mut [[i64; N]; N]) {
    for i in 0..N {
        for j in 0..N {
            a[i][j] = ((i * (j + 2)) as i64 + 2) / N as i64;
            b[i][j] = ((i * (j + 3)) as i64 + 3) / N as i64;
        }
    }
}

fn kernel(a: &mut [[i64; N]; N], b: &mut [[i64; N]; N]) {
    for _ in 0..TSTEPS {
        for i in 1..N - 1 {
            for j in 1..N - 1 {
                b[i][j] = (a[i][j] + a[i][j - 1] + a[i][j + 1] + a[i + 1][j] + a[i - 1][j]) / 5;
            }
        }
        for i in 1..N - 1 {
            for j in 1..N - 1 {
                a[i][j] = b[i][j];
            }
        }
    }
}

fn checksum(a: &[[i64; N]; N]) -> i64 {
    let mut s = 0;
    for row in a {
        for v in row {
            s += *v;
        }
    }
    s
}

fn main() {
    let mut a = [[0i64; N]; N];
    let mut b = [[0i64; N]; N];
    init_array(&mut a, &mut b);
    kernel(&mut a, &mut b);
    eprintln!("checksum={}", checksum(&a));
}
