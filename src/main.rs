use eframe::egui;
use egui::Color32;
use num_complex::{Complex64, ComplexFloat};
use rayon::prelude::*;

fn fractal_depth(
    mut z: Complex64,
    c: Complex64,
    o: f64,
    p: Complex64,
    depth: usize,
) -> (usize, Complex64) {
    let mut last = Complex64::ZERO;
    let bound = if o < 0.0 {
        2.0 * std::f64::consts::SQRT_2
    } else {
        2.0
    };
    for i in 0..depth {
        if z.abs() > bound {
            return (i, z);
        }
        let old_z = z;
        z = z.powf(o) + p * last + c;
        last = old_z;
    }
    (depth, z)
}

#[derive(Clone)]
struct Palette {
    edit: Vec<(Color32, f32)>,
    stops: Vec<(Color32, f32)>,
    exponential: bool,
    palette: Vec<Color32>,
}
impl Palette {
    pub fn bw() -> Self {
        Self {
            edit: vec![(Color32::BLACK, 0.0), (Color32::WHITE, 1.0)],
            stops: vec![(Color32::BLACK, 0.0), (Color32::WHITE, 1.0)],
            exponential: false,
            palette: Vec::new(),
        }
    }
    pub fn regenerate(&mut self, depth: usize, sort: bool) {
        self.stops.clone_from(&self.edit);
        self.palette.clear();
        self.palette.reserve(depth + 1);
        if sort {
            self.stops.sort_by(|a, b| a.1.total_cmp(&b.1));
        }
        for i in 0..=depth {
            let d = if self.exponential {
                1.0 - 0.5f32.powf(i as f32 / depth as f32)
            } else {
                i as f32 / depth as f32
            };
            let idx = self.stops.binary_search_by(|(_, k)| k.total_cmp(&d));
            match idx {
                Ok(idx) => self.palette.push(self.stops[idx].0),
                Err(idx) => {
                    if idx == 0 {
                        self.palette.push(self.stops[0].0);
                    } else if idx == self.stops.len() {
                        self.palette.push(self.stops[self.stops.len() - 1].0);
                    } else {
                        let (a, ka) = self.stops[idx - 1];
                        let (b, kb) = self.stops[idx];
                        self.palette.push(a.lerp_to_gamma(b, (d - ka) / (kb - ka)));
                    }
                }
            }
        }
    }
    pub fn respace(&mut self) {
        self.edit.sort_by(|a, b| a.1.total_cmp(&b.1));
        let step = ((self.edit.len() - 1) as f32).recip();
        let mut val = 0.0;
        for (_color, k) in &mut self.edit {
            *k = val;
            val += step;
        }
    }
}

fn show_picker(ui: &mut egui::Ui, palette: &mut Palette) -> bool {
    let mut changed = ui
        .checkbox(&mut palette.exponential, "Exponential")
        .changed();
    if ui.button("Respace").clicked() {
        palette.respace();
        changed = true;
    }
    let mut to_remove = Vec::new();
    let can_remove = palette.edit.len() > 1;
    for (i, (color, k)) in palette.edit.iter_mut().enumerate() {
        ui.horizontal(|ui| {
            changed |= ui.color_edit_button_srgba(color).changed();
            if ui
                .add(egui::Slider::new(k, 0.0..=1.0).text("Stop"))
                .changed()
            {
                changed = true;
            }
            if ui.add_enabled(can_remove, egui::Button::new("X")).clicked() {
                to_remove.push(i);
                changed = true;
            }
        });
    }
    let mut i = 0;
    palette.edit.retain(|_| {
        if to_remove.contains(&i) {
            i += 1;
            false
        } else {
            i += 1;
            true
        }
    });
    if ui.button("Add Stop").clicked() {
        palette.edit.push((Color32::WHITE, 1.0));
        changed = true;
    }
    changed
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
    let mut renorm = false;
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
    let mut upper_palette = Palette::bw();
    let mut lower_palette = None::<Palette>;
    upper_palette.regenerate(depth, false);
    eframe::run_simple_native("Hyperjulia", Default::default(), move |ctx, _| {
        egui::Window::new("Display").show(ctx, |ui| {
            if ui
                .add(
                    egui::Slider::new(&mut multibrot_resolution, 10..=1000)
                        .logarithmic(true)
                        .text("Multibrot Resolution"),
                )
                .changed()
            {
                update_multibrot = true;
            }
            if ui
                .add(
                    egui::Slider::new(&mut hyperjulia_resolution, 10..=1000)
                        .logarithmic(true)
                        .text("Hyperjulia Resolution"),
                )
                .changed()
            {
                update_hyperjulia = true;
            }
            if ui.checkbox(&mut show_marker, "Show Marker").changed() {
                update_multibrot = true;
            }
            if ui.checkbox(&mut renorm, "Renorm Hyperjulia").changed() {
                update_hyperjulia = true;
            }
        });
        egui::Window::new("Params").show(ctx, |ui| {
            ui.horizontal(|ui| {
                if ui
                    .add(
                        egui::Slider::new(&mut exponent, -5.0..=5.0)
                            .max_decimals(if integer_exp { 0 } else { 2 })
                            .text("Exponent"),
                    )
                    .changed()
                {
                    update_multibrot = true;
                    update_hyperjulia = true;
                }
                ui.checkbox(&mut integer_exp, "Integer");
            });
            ui.horizontal(|ui| {
                ui.label("c: ");
                ui.vertical(|ui| {
                    if ui
                        .add(egui::TextEdit::singleline(&mut c_str).desired_width(50.0))
                        .changed()
                    {
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
                        ui.label(
                            egui::RichText::new("Invalid parameter")
                                .color(visuals.error_fg_color)
                                .background_color(visuals.extreme_bg_color),
                        );
                    }
                });
            });
            ui.horizontal(|ui| {
                ui.label("P: ");
                ui.vertical(|ui| {
                    if ui
                        .add(egui::TextEdit::singleline(&mut p_str).desired_width(50.0))
                        .changed()
                    {
                        if let Ok(new_p) = p_str.parse() {
                            phoenix = new_p;
                            update_multibrot = true;
                            update_hyperjulia = true;
                            invalid_p = false;
                        } else {
                            invalid_p = true;
                        }
                    }
                    if invalid_p {
                        let visuals = &ui.style().visuals;
                        ui.label(
                            egui::RichText::new("Invalid parameter")
                                .color(visuals.error_fg_color)
                                .background_color(visuals.extreme_bg_color),
                        );
                    }
                });
            });
            if ui
                .add(
                    egui::Slider::new(&mut depth, 1..=1000)
                        .logarithmic(true)
                        .text("Depth"),
                )
                .changed()
            {
                upper_palette.regenerate(depth, false);
                if let Some(lower) = &mut lower_palette {
                    lower.regenerate(depth, false);
                }
                update_multibrot = true;
                update_hyperjulia = true;
            }
        });
        egui::Window::new("Colors").show(ctx, |ui| {
            let mut changed = ui
                .collapsing(
                    if lower_palette.is_some() {
                        "Upper"
                    } else {
                        "Gradient"
                    },
                    |ui| show_picker(ui, &mut upper_palette),
                )
                .body_returned
                .unwrap_or(false);
            let mut show_lower = lower_palette.is_some();
            changed |= ui.checkbox(&mut show_lower, "Lower").changed();
            if show_lower {
                let lower = lower_palette.get_or_insert_with(|| upper_palette.clone());
                changed |= ui
                    .collapsing("Lower", |ui| show_picker(ui, lower))
                    .body_returned
                    .unwrap_or(false);
            } else {
                lower_palette = None;
            }
            if changed {
                upper_palette.regenerate(depth, true);
                if let Some(lower) = &mut lower_palette {
                    lower.regenerate(depth, true);
                }
                update_multibrot = true;
                update_hyperjulia = true;
            }
        });
        let multibrot = multibrot_handle.get_or_insert_with(|| {
            ctx.load_texture(
                "multibrot",
                egui::ColorImage::example(),
                egui::TextureOptions::NEAREST,
            )
        });
        let hyperjulia = hyperjulia_handle.get_or_insert_with(|| {
            ctx.load_texture(
                "hyperjulia",
                egui::ColorImage::example(),
                egui::TextureOptions::NEAREST,
            )
        });
        if update_multibrot {
            update_multibrot = false;
            multibrot_buffer.resize(
                multibrot_resolution * multibrot_resolution,
                Color32::PLACEHOLDER,
            );
            let scale = 4.0 / (multibrot_resolution as f64);
            let target = show_marker.then(|| {
                (
                    ((c.re + 2.0) / scale) as usize,
                    ((-c.im + 2.0) / scale) as usize,
                )
            });
            multibrot_buffer
                .par_iter_mut()
                .enumerate()
                .for_each(|(n, px)| {
                    let (y, x) = num_integer::div_rem(n, multibrot_resolution);
                    if let Some((tx, ty)) = target {
                        if (x == tx && y.abs_diff(ty) < 5) || (y == ty && x.abs_diff(tx) < 5) {
                            *px = Color32::RED;
                            return;
                        }
                    }
                    let x = x as f64 * scale - 2.0;
                    let y = y as f64 * scale - 2.0;
                    let c = Complex64::new(x, -y);
                    let (d, _) =
                        fractal_depth(Complex64::ZERO, c, exponent, phoenix, depth);
                    let upper = upper_palette.palette[d];
                    let color = if let Some(lower) = &lower_palette {
                        upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
                    } else {
                        upper
                    };
                    *px = color;
                });
            multibrot.set(
                egui::ColorImage {
                    size: [multibrot_resolution; 2],
                    pixels: multibrot_buffer.clone(),
                },
                egui::TextureOptions::NEAREST,
            )
        }
        if update_hyperjulia {
            update_hyperjulia = false;
            hyperjulia_buffer.resize(
                hyperjulia_resolution * hyperjulia_resolution,
                Color32::PLACEHOLDER,
            );
            let scale = 4.0 / (hyperjulia_resolution as f64);
            hyperjulia_buffer
                .par_iter_mut()
                .enumerate()
                .for_each(|(n, px)| {
                    let (y, x) = num_integer::div_rem(n, hyperjulia_resolution);
                    let x = x as f64 * scale - 2.0;
                    let y = y as f64 * scale - 2.0;
                    let z = Complex64::new(x, -y);
                    let (d, z) = fractal_depth(z, c, exponent, phoenix, depth);
                    let d = if renorm {
                        (((d + 1) as f64 - z.abs().max(1.0).ln().max(1.0).ln())
                            / std::f64::consts::LN_2) as usize
                    } else {
                        d
                    };
                    let upper = upper_palette.palette[d];
                    let color = if let Some(lower) = &lower_palette {
                        upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
                    } else {
                        upper
                    };
                    *px = color;
                });
            hyperjulia.set(
                egui::ColorImage {
                    size: [hyperjulia_resolution; 2],
                    pixels: hyperjulia_buffer.clone(),
                },
                egui::TextureOptions::NEAREST,
            )
        }
        egui::Window::new("Multibrot").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("c: ");
                ui.vertical(|ui| {
                    if ui
                        .add(egui::TextEdit::singleline(&mut c_str).desired_width(50.0))
                        .changed()
                    {
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
                        ui.label(
                            egui::RichText::new("Invalid parameter")
                                .color(visuals.error_fg_color)
                                .background_color(visuals.extreme_bg_color),
                        );
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
            let img = ui.image(&*hyperjulia);
            img.context_menu(|ui| {
                if ui.button("Save").clicked() {
                    let dialog = rfd::AsyncFileDialog::new()
                        .add_filter("Images", &["png"])
                        .set_file_name("hyperjulia.png");
                    let buf = hyperjulia_buffer.clone();
                    std::thread::spawn(move || {
                        let Some(handle) = futures_lite::future::block_on(dialog.save_file())
                        else {
                            return;
                        };
                        let Ok(file) = std::fs::File::create(handle.path())
                            .inspect_err(|err| eprintln!("Failed to open file: {err}"))
                        else {
                            return;
                        };
                        let mut encoder = png::Encoder::new(
                            file,
                            hyperjulia_resolution as _,
                            hyperjulia_resolution as _,
                        );
                        encoder.set_color(png::ColorType::Rgba);
                        let Ok(mut writer) = encoder
                            .write_header()
                            .inspect_err(|err| eprintln!("Failed to write header: {err}"))
                        else {
                            return;
                        };
                        if let Err(err) = writer.write_image_data(bytemuck::cast_slice(&buf)) {
                            eprintln!("Failed to write image data: {err}");
                        }
                        if let Err(err) = writer.finish() {
                            eprintln!("Failed to finish writing: {err}");
                        }
                    });
                }
            });
        });
    })
    .unwrap();
}
