use clap::Parser;
use num_complex::{Complex64, ComplexFloat};
use std::path::PathBuf;
use tiny_skia::*;

#[derive(Parser)]
struct Cli {
    c: Complex64,
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

fn make_palette(colors: &[u32], size: usize) -> Vec<u32> {
    let mut out = vec![0; size];
    let lcm = num_integer::lcm(colors.len() - 1, size);
    let cs = lcm / (colors.len() - 1);
    let os = lcm / size;
    for (i, c) in out.iter_mut().enumerate() {
        let exp = i * os;
        let (div, rem) = num_integer::div_rem(exp, cs);
        if rem == 0 || div == colors.len() {
            *c = colors[div];
        } else {
            *c = lerp(colors[div], colors[div + 1], (rem as f64) / (cs as f64));
        }
    }
    out
}

fn julia_depth(mut z: Complex64, c: Complex64, depth: usize) -> usize {
    for i in 0..depth {
        if z.abs() > 2.0 {
            return i;
        }
        z = z * z + c;
    }
    depth
}

fn main() {
    const UPPER_PALETTE: &[u32] = &[0x9c59d1ff, 0xfcf434ff];
    const LOWER_PALETTE: &[u32] = &[0x2c2c2cff, 0xffffffff];
    let cli = Cli::parse();
    let mut canvas = tiny_skia::Pixmap::new(cli.res, cli.res).unwrap();
    let upper = make_palette(UPPER_PALETTE, cli.depth + 1);
    let lower = make_palette(LOWER_PALETTE, cli.depth + 1);
    let scale = 4.0 / (cli.res as f64);
    for (i, px) in canvas.pixels_mut().iter_mut().enumerate() {
        let (y, x) = num_integer::div_rem(i, cli.res as usize);
        let x = x as f64 * scale - 2.0;
        let y = y as f64 * scale - 2.0;
        let z = Complex64::new(x, y);
        let depth = julia_depth(z, cli.c, cli.depth);
        let color = lerp(upper[depth], lower[depth], y / 4.0 + 0.5);
        *px = make_color(color).premultiply();
    }
    canvas.save_png(cli.output).unwrap();
}
