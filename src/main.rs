use clap::Parser;
use num_complex::{Complex64, ComplexFloat};
use std::path::PathBuf;
use tiny_skia::*;

#[derive(Parser)]
struct Cli {
    c: Complex64,
    #[arg(short, long, default_value_t = 2.0)]
    exponent: f64,
    #[arg(short = 'P', long, default_value_t = Complex64::ZERO)]
    phoenix: Complex64,
    #[arg(short, long)]
    output: PathBuf,
    #[arg(short, long)]
    res: u32,
    #[arg(short, long)]
    depth: usize,
}

fn make_color(c: u32) -> ColorU8 {
    let [r, g, b, a] = c.to_be_bytes();
    ColorU8::from_rgba(r, g, b, a)
}

fn lerp(a: u32, b: u32, p: f64) -> u32 {
    let [ar, ag, ab, aa] = a.to_be_bytes();
    let [br, bg, bb, ba] = b.to_be_bytes();
    u32::from_be_bytes([
        (br as f64 * p + ar as f64 * (1.0 - p)).floor() as u8,
        (bg as f64 * p + ag as f64 * (1.0 - p)).floor() as u8,
        (bb as f64 * p + ab as f64 * (1.0 - p)).floor() as u8,
        (ba as f64 * p + aa as f64 * (1.0 - p)).floor() as u8,
    ])
}

fn sample_palette(colors: &[u32], pos: f64) -> u32 {
    match colors {
        [] => panic!(),
        &[c] => c,
        &[a, b] => lerp(a, b, pos),
        _ => {
            let frac = (colors.len() as f64).recip();
            let base = pos / frac;
            let rem = base.fract() * (colors.len() as f64);
            let base = base as usize;
            if rem == 0.0 {
                colors[base]
            } else {
                lerp(colors[base], colors[base + 1], rem)
            }
        }
    }
}

fn depth(mut z: Complex64, c: Complex64, o: f64, p: Complex64, depth: usize) -> (usize, Complex64) {
    let mut last = Complex64::ZERO;
    for i in 0..depth {
        if z.abs() > 2.0 {
            return (i, z);
        }
        let old_z = z;
        z = z.powf(o) + p * last + c;
        last = old_z;
    }
    (depth, z)
}

fn main() {
    let cli = Cli::parse();
    let mut canvas = tiny_skia::Pixmap::new(cli.res, cli.res).unwrap();
    let scale = 4.0 / (cli.res as f64);
    for (i, px) in canvas.pixels_mut().iter_mut().enumerate() {
        let (y, x) = num_integer::div_rem(i, cli.res as usize);
        let x = x as f64 * scale - 2.0;
        let y = y as f64 * scale - 2.0;
        let z = Complex64::new(x, y);
        let (depth, z) = depth(z, cli.c, cli.exponent, cli.phoenix, cli.depth);
        let renormed = ((depth + 1) as f64
            - z.abs().max(1.0).ln().max(1.0).ln() / std::f64::consts::LN_2)
            / cli.depth as f64;
        let upper = sample_palette(&[0x9c59d1ff, 0xfcf434ff], renormed);
        let lower = sample_palette(&[0x2c2c2cff, 0xffffffff], renormed);
        let color = lerp(upper, lower, y / 4.0 + 0.5);
        *px = make_color(color).premultiply();
    }
    canvas.save_png(cli.output).unwrap();
}
