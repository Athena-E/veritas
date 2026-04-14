use std::time::Instant;

const N: usize = 1024;

fn init_array(path: &mut Vec<Vec<i64>>) {
    for i in 0..N {
        for j in 0..N {
            path[i][j] = ((i as i64 + 1) * (j as i64 + 1)) / N as i64;
        }
    }
}

fn kernel(path: &mut Vec<Vec<i64>>) {
    for k in 0..N {
        for i in 0..N {
            for j in 0..N {
                let via = path[i][k] + path[k][j];
                if via < path[i][j] {
                    path[i][j] = via;
                }
            }
        }
    }
}

fn checksum(path: &Vec<Vec<i64>>) -> i64 {
    let mut s: i64 = 0;
    for i in 0..N {
        for j in 0..N {
            s = s.wrapping_add(path[i][j]);
        }
    }
    s
}

fn main() {
    let mut path = vec![vec![0i64; N]; N];
    init_array(&mut path);

    let t0 = Instant::now();
    kernel(&mut path);
    let elapsed = t0.elapsed().as_secs_f64();

    eprintln!("checksum={}", checksum(&path));
    println!("{:.6}", elapsed);
}
