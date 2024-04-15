use clap::Parser;
use num::complex::{Complex64, ComplexFloat};
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
    #[arg(short, long)]
    palette: String,
}

fn lerp(a: u32, b: u32, p: f64) -> PremultipliedColorU8 {
    let [ar, ag, ab, aa] = a.to_be_bytes();
    let [br, bg, bb, ba] = b.to_be_bytes();
    tiny_skia::ColorU8::from_rgba(
        (br as f64 * p + ar as f64 * (1.0 - p)).floor() as u8,
        (bg as f64 * p + ag as f64 * (1.0 - p)).floor() as u8,
        (bb as f64 * p + ab as f64 * (1.0 - p)).floor() as u8,
        (ba as f64 * p + aa as f64 * (1.0 - p)).floor() as u8,
    ).premultiply()
}

fn make_palette(colors: &[u32], size: usize) -> Vec<PremultipliedColorU8> {
    let mut out = vec![PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap(); size];
    let lcm = num::integer::lcm(colors.len() - 1, size);
    let cs = lcm / (colors.len() - 1);
    let os = lcm / size;
    for (i, c) in out.iter_mut().enumerate() {
        let exp = i * os;
        let (div, rem) = num::integer::div_rem(exp, cs);
        if rem == 0 || div == colors.len() {
            let [r, g, b, a] = colors[div].to_be_bytes();
            *c = ColorU8::from_rgba(r, g, b, a).premultiply();
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

fn parse_palette(mut input: &str) -> Vec<u32> {
    if input.starts_with('@') {
        parse_palette(&std::fs::read_to_string(&input[1..]).unwrap())
    } else {
        let mut out = Vec::with_capacity(input.len() / 8);
        loop {
            input = input.trim_start();
            let len = input.bytes().take(9).position(|i| !i.is_ascii_hexdigit()).unwrap_or(std::cmp::min(input.len(), 9));
            let c = match len {
                0 => break,
                6 => (u32::from_str_radix(&input[..6], 16).unwrap() << 8) | 0xFF,
                8 => u32::from_str_radix(&input[..8], 16).unwrap(),
                l => panic!("expected 6 or 8 character input, found {l}!"),
            };
            input = &input[len..];
            out.push(c);
        }
        out
    }
}

fn main() {
    let cli = Cli::parse();
    let mut canvas = tiny_skia::Pixmap::new(cli.res, cli.res).unwrap();
    let palette = make_palette(&parse_palette(&cli.palette), cli.depth + 1);
    let scale = 4.0 / (cli.res as f64);
    for (i, px) in canvas.pixels_mut().iter_mut().enumerate() {
        let (y, x) = num::integer::div_rem(i, cli.res as usize);
        let z = Complex64::new(x as f64 * scale - 2.0, y as f64 * scale - 2.0);
        let depth = julia_depth(z, cli.c, cli.depth);
        *px = palette[depth];
    }
    canvas.save_png(cli.output).unwrap();
}
