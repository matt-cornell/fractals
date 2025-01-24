use num_complex::{Complex64, ComplexFloat};
use eframe::egui;
use egui::Color32;

fn make_color(c: u32) -> Color32 {
    let [r, g, b, a] = c.to_be_bytes();
    Color32::from_rgba_unmultiplied(r, g, b, a)
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

fn fractal_depth(mut z: Complex64, c: Complex64, o: f64, p: Complex64, depth: usize) -> (usize, Complex64) {
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
    let mut multibrot_resolution = 256;
    let mut hyperjulia_resolution = 512;
    let mut c = Complex64::ZERO;
    let mut exponent = 2.0;
    let mut phoenix = Complex64::ZERO;
    let mut depth = 100usize;
    let mut integer_exp = true;
    let mut show_marker = true;
    let mut multibrot_buffer = Vec::new();
    let mut hyperjulia_buffer = Vec::new();
    let mut multibrot_handle = None;
    let mut hyperjulia_handle = None;
    let mut update_multibrot = true;
    let mut update_hyperjulia = true;
    let mut c_str = c.to_string();
    let mut p_str = phoenix.to_string();
    let mut invalid_c = false;
    let mut invalid_p = false;
    eframe::run_simple_native("Hyperjulia", Default::default(), move |ctx, _| {
        egui::Window::new("Display").show(ctx, |ui| {
            if ui.add(egui::Slider::new(&mut multibrot_resolution, 10..=1000).logarithmic(true).text("Multibrot Resolution")).changed() {
                update_multibrot = true;
            }
            if ui.add(egui::Slider::new(&mut hyperjulia_resolution, 10..=1000).logarithmic(true).text("Hyperjulia Resolution")).changed() {
                update_hyperjulia = true;
            }
            if ui.checkbox(&mut show_marker, "Show Marker").changed() {
                update_multibrot = true;
            }
        });
        egui::Window::new("Params").show(ctx, |ui| {
            ui.horizontal(|ui| {
                if ui.add(egui::Slider::new(&mut exponent, -5.0..=5.0).max_decimals(if integer_exp { 0 } else { 2 }).text("Exponent")).changed() {
                    update_multibrot = true;
                    update_hyperjulia = true;
                }
                ui.checkbox(&mut integer_exp, "Integer");
            });
            ui.horizontal(|ui| {
                ui.label("c: ");
                ui.vertical(|ui| {
                    if ui.text_edit_singleline(&mut c_str).changed() {
                        if let Ok(new_c) = c_str.parse() {
                            c = new_c;
                            update_hyperjulia = true;
                            if show_marker {
                                update_multibrot = true;
                            }
                            invalid_c = false;
                        } else {
                            invalid_c = true;
                        }
                    }
                    if invalid_c {
                        let visuals = &ui.style().visuals;
                        ui.label(egui::RichText::new("Invalid parameter").color(visuals.error_fg_color).background_color(visuals.extreme_bg_color));
                    }
                });
            });
            ui.horizontal(|ui| {
                ui.label("P: ");
                ui.vertical(|ui| {
                    if ui.text_edit_singleline(&mut p_str).changed() {
                        if let Ok(new_p) = p_str.parse() {
                            phoenix = new_p;
                            update_hyperjulia = true;
                        } else {
                            let visuals = &ui.style().visuals;
                            ui.label(egui::RichText::new("Invalid parameter").color(visuals.error_fg_color).background_color(visuals.extreme_bg_color));
                        }
                    }
                });
            });
        });
        let multibrot = multibrot_handle.get_or_insert_with(|| ctx.load_texture("multibrot", egui::ColorImage::example(), egui::TextureOptions::NEAREST));
        let hyperjulia = hyperjulia_handle.get_or_insert_with(|| ctx.load_texture("hyperjulia", egui::ColorImage::example(), egui::TextureOptions::NEAREST));
        if update_multibrot {
            update_multibrot = false;
            multibrot_buffer.resize(multibrot_resolution * multibrot_resolution, Color32::PLACEHOLDER);
            let scale = 4.0 / (multibrot_resolution as f64);
            let target = show_marker.then(|| (
                ((c.re + 2.0) / scale) as usize, 
                ((-c.im + 2.0) / scale) as usize, 
            ));
            for (n, px) in multibrot_buffer.iter_mut().enumerate() {
                let (y, x) = num_integer::div_rem(n, multibrot_resolution);
                if let Some((tx, ty)) = target {
                    if (x == tx && y.abs_diff(ty) < 5) || (y == ty && x.abs_diff(tx) < 5) {
                        *px = Color32::RED;
                        continue;
                    }
                }
                let x = x as f64 * scale - 2.0;
                let y = y as f64 * scale - 2.0;
                let c = Complex64::new(x, -y);
                let (d, _) = fractal_depth(Complex64::ZERO, c, exponent, Complex64::ZERO, depth);
                let d = d as f64 / depth as f64;
                *px = Color32::from_gray((d.sqrt() * 255.0) as u8);
            }
            multibrot.set(
                egui::ColorImage {
                    size: [multibrot_resolution; 2],
                    pixels: multibrot_buffer.clone(),
                },
                egui::TextureOptions::NEAREST
            )
        }
        if update_hyperjulia {
            update_hyperjulia = false;
            hyperjulia_buffer.resize(hyperjulia_resolution * hyperjulia_resolution, Color32::PLACEHOLDER);
            let scale = 4.0 / (hyperjulia_resolution as f64);
            for (n, px) in hyperjulia_buffer.iter_mut().enumerate() {
                let (y, x) = num_integer::div_rem(n, hyperjulia_resolution);
                let x = x as f64 * scale - 2.0;
                let y = y as f64 * scale - 2.0;
                let z = Complex64::new(x, -y);
                let (d, z) = fractal_depth(z, c, exponent, phoenix, depth);
                let d = d as f64 / depth as f64;
                // let d = (d - z.abs().max(1.0).ln().max(1.0).ln() / std::f64::consts::LN_2);
                *px = Color32::from_gray((d * 255.0) as u8);
            }
            hyperjulia.set(
                egui::ColorImage {
                    size: [hyperjulia_resolution; 2],
                    pixels: hyperjulia_buffer.clone(),
                },
                egui::TextureOptions::NEAREST
            )
        }
        egui::Window::new("Multibrot").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("c: ");
                ui.vertical(|ui| {
                    if ui.text_edit_singleline(&mut c_str).changed() {
                        if let Ok(new_c) = c_str.parse() {
                            c = new_c;
                            update_hyperjulia = true;
                            if show_marker {
                                update_multibrot = true;
                            }
                            invalid_c = false;
                        } else {
                            invalid_c = true;
                        }
                    }
                    if invalid_c {
                        let visuals = &ui.style().visuals;
                        ui.label(egui::RichText::new("Invalid parameter").color(visuals.error_fg_color).background_color(visuals.extreme_bg_color));
                    }
                });
            });
            let mut img = ui.image(&*multibrot);
            if show_marker {
                img = img.interact(egui::Sense::click_and_drag());
                let scale = 4.0 / (multibrot_resolution as f64);
                if let Some(pos) = img.interact_pointer_pos() {
                    let p = pos - img.rect.min;
                    let x = p.x as f64 * scale - 2.0;
                    let y = p.y as f64 * scale - 2.0;
                    c = Complex64::new((x * 100.0).round() * 0.01, (y * 100.0).round() * -0.01);
                    update_hyperjulia = true;
                    update_multibrot = true;
                    c_str = c.to_string();
                }
            }
        });
        egui::Window::new("Hyperjulia").show(ctx, |ui| {
            ui.image(&*hyperjulia);
        });
    }).unwrap();
    /*let mut canvas = tiny_skia::Pixmap::new(cli.res, cli.res).unwrap();
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
    canvas.save_png(cli.output).unwrap();*/
}
