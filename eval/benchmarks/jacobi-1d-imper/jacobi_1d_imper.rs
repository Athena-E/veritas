const N: usize = 128;
const TSTEPS: usize = 80;

fn init_array(a: &mut [i64; N], b: &mut [i64; N]) {
    for i in 0..N {
        a[i] = (i as i64 + 2) / N as i64;
        b[i] = (i as i64 + 3) / N as i64;
    }
}

fn kernel(a: &mut [i64; N], b: &mut [i64; N]) {
    for _ in 0..TSTEPS {
        for i in 1..N - 1 {
            b[i] = (a[i - 1] + a[i] + a[i + 1]) / 3;
        }
        a[1..(N - 1)].copy_from_slice(&b[1..(N - 1)]);
    }
}

fn checksum(a: &[i64; N]) -> i64 {
    a.iter().copied().sum()
}

fn main() {
    let mut a = [0i64; N];
    let mut b = [0i64; N];
    init_array(&mut a, &mut b);
    kernel(&mut a, &mut b);
    eprintln!("checksum={}", checksum(&a));
}
