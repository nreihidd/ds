use render;
use na::{Vector2, Vector4};
use std::rc::Rc;

use rusttype::{self, PositionedGlyph};
use ::FONT;

const FONT_SCALE: rusttype::Scale = rusttype::Scale { x: 16.0, y: 16.0 };

#[derive(Clone)]
pub struct LayoutNode {
    layout: Box<Layout>,
    size: Vector2<f32>,
}

type BLayout = Box<LayoutNode>;
#[derive(Clone)]
pub enum Layout {
    Horizontal(BLayout, BLayout),
    Vertical(BLayout, BLayout, f32),
    DbgTreeHorizontal(BLayout, BLayout),
    DbgTreeVertical(BLayout, BLayout),
    Table(Vec<Vec<LayoutNode>>),
    Background(BLayout, Vector4<f32>),
    Border(BLayout, Vector2<f32>, Vector4<f32>),
    Text(Vec<PositionedGlyph<'static>>, Vector4<f32>),
    Empty(Vector2<f32>),
    NavLeft(BLayout),
    NavRight(BLayout),
    NavCompound(BLayout),
}

#[derive(Debug, Eq, PartialEq)]
pub enum LayoutPath {
    Left(Rc<LayoutPath>),
    Right(Rc<LayoutPath>),
    Value(Rc<LayoutPath>),
    End
}

fn contains(p: Vector2<f32>, size: Vector2<f32>, target: Vector2<f32>) -> bool {
    target.x >= p.x && target.x <= p.x + size.x && target.y >= p.y && target.y <= p.y + size.y
}

fn union(a: rusttype::Rect<i32>, b: rusttype::Rect<i32>) -> rusttype::Rect<i32> {
    use std::cmp::{min, max};
    rusttype::Rect {
        min: rusttype::Point {
            x: min(a.min.x, b.min.x),
            y: min(a.min.y, b.min.y),
        },
        max: rusttype::Point {
            x: max(a.max.x, b.max.x),
            y: max(a.max.y, b.max.y)
        }
    }
}

struct TableDimensions {
    row_heights: Vec<f32>,
    col_widths: Vec<f32>,
}
fn table_dimensions(entries: &Vec<Vec<LayoutNode>>) -> TableDimensions {
    let row_heights: Vec<f32> = entries.iter().map(|row| row.iter().fold(0.0, |acc: f32, entry| acc.max(entry.size.y))).collect();
    let num_columns: usize = entries.iter().map(|row| row.len()).max().unwrap_or(0);
    let col_widths: Vec<f32> = (0..num_columns).map(|col| entries.iter().map(|row| row.get(col).map(|entry| entry.size.x).unwrap_or(0.0)).fold(0.0, |acc: f32, x| acc.max(x))).collect();
    TableDimensions { 
        row_heights: row_heights,
        col_widths: col_widths,
    }
}

fn text_bounds(glyphs: &[PositionedGlyph<'static>]) -> rusttype::Rect<i32> {
    use rusttype::{Rect, Point};
    let v_metrics = FONT.v_metrics(FONT_SCALE);
    let high_y = 0 - (v_metrics.descent * 0.75).floor() as i32;
    let low_y = 0 - (v_metrics.ascent * 0.75).ceil() as i32;
    let bbox = glyphs.iter()
        .filter_map(|glyph| glyph.pixel_bounding_box())
        .fold(Rect { min: Point { x: 0, y: 0 }, max: Point { x: 0, y: 0 } }, union);
    let padding = 1;
    union(
        Rect { min: Point { x: bbox.min.x - padding, y: bbox.min.y - padding }, max: Point { x: bbox.max.x + padding, y: bbox.max.y + padding } },
        Rect { min: Point { x: 0, y: low_y }, max: Point { x: 0, y: high_y } }
    )
}

pub fn make_text(msg: &str, color: Vector4<f32>) -> Layout {
    let glyphs = FONT.layout(msg, FONT_SCALE, rusttype::Point { x: 0.0, y: 0.0 }).collect();
    Layout::Text(glyphs, color)
}

impl LayoutNode {
    pub fn new(layout: Layout) -> LayoutNode {
        let size = match layout {
            Layout::DbgTreeHorizontal(ref left, ref right) => Vector2::new(left.size.x + right.size.x, left.size.y.max(right.size.y) + 5.0),
            Layout::Horizontal(ref left, ref right) => Vector2::new(left.size.x + right.size.x, left.size.y.max(right.size.y)),
            Layout::DbgTreeVertical(ref top, ref bottom) => Vector2::new(top.size.x.max(bottom.size.x) + 5.0, top.size.y + bottom.size.y),
            Layout::Vertical(ref top, ref bottom, _align) => Vector2::new(top.size.x.max(bottom.size.x), top.size.y + bottom.size.y),
            Layout::Background(ref e, _) => e.size,
            Layout::Border(ref e, border_size, _) => Vector2::new(e.size.x + border_size.x * 2.0, e.size.y + border_size.y * 2.0),
            Layout::Text(ref s, _) => {
                let bbox = text_bounds(s);
                // The .max(5.0)'s are to enforce a minimum size for the empty string. (Otherwise it would have size 0.0x0.0 and what would even)
                Vector2::new(((bbox.max.x - bbox.min.x) as f32).max(5.0), ((bbox.max.y - bbox.min.y) as f32).max(5.0))
            },
            Layout::Empty(size) => size,
            Layout::Table(ref entries) => {
                let dimensions = table_dimensions(entries);
                Vector2::new(dimensions.col_widths.iter().fold(0.0, |acc, x| acc + x), dimensions.row_heights.iter().fold(0.0, |acc, x| acc + x))
            },
            Layout::NavLeft(ref inner) | Layout::NavRight(ref inner) | Layout::NavCompound(ref inner) => inner.size
        };
        LayoutNode {
            layout: Box::new(layout),
            size: size,
        }
    }
    pub fn draw<T: render::GeometryGenerator>(&self, offset: Vector2<f32>, draw: &mut T) {
        // if offset.x.fract() != 0.0 { println!("Non-integer offset: {}", offset.x); }
        // if offset.y.fract() != 0.0 { println!("Non-integer offset: {}", offset.y); }
        match *self.layout {
            Layout::Horizontal(ref left, ref right) => {
                left.draw(Vector2::new(offset.x, offset.y + ((self.size.y - left.size.y) / 2.0).floor()), draw);
                right.draw(Vector2::new(offset.x + left.size.x, offset.y + ((self.size.y - right.size.y) / 2.0).floor()), draw);
            },
            Layout::DbgTreeHorizontal(ref left, ref right) => {
                let mid = (self.size.x / 2.0).floor();
                let mid_left = (left.size.x / 2.0).floor();
                let mid_right = left.size.x + (right.size.x / 2.0).floor();

                fn draw_quad<T: render::GeometryGenerator>(draw: &mut T, top_left: Vector2<f32>, size: Vector2<f32>, color: Vector4<f32>) {
                    draw.draw_quad(top_left + size / 2.0, size, color);
                }

                draw_quad(draw, Vector2::new(offset.x + mid_left, offset.y + 2.0), Vector2::new(mid_right - mid_left + 1.0, 1.0), Vector4::new(0.0, 0.0, 0.0, 1.0));
                draw_quad(draw, Vector2::new(offset.x + mid, offset.y), Vector2::new(1.0, 2.0), Vector4::new(0.0, 0.0, 0.0, 1.0));
                draw_quad(draw, Vector2::new(offset.x + mid_left, offset.y + 3.0), Vector2::new(1.0, 2.0), Vector4::new(0.0, 0.0, 0.0, 1.0));
                draw_quad(draw, Vector2::new(offset.x + mid_right, offset.y + 3.0), Vector2::new(1.0, 2.0), Vector4::new(0.0, 0.0, 0.0, 1.0));

                left.draw(Vector2::new(offset.x, offset.y + 5.0), draw);
                right.draw(Vector2::new(offset.x + left.size.x, offset.y + 5.0), draw);
            },
            Layout::DbgTreeVertical(ref top, ref bottom) => {
                let mid = (self.size.y / 2.0).floor();
                let mid_top = (top.size.y / 2.0).floor();
                let mid_bot = top.size.y + (bottom.size.y / 2.0).floor();

                fn draw_quad<T: render::GeometryGenerator>(draw: &mut T, top_left: Vector2<f32>, size: Vector2<f32>, color: Vector4<f32>) {
                    draw.draw_quad(top_left + size / 2.0, size, color);
                }

                draw_quad(draw, Vector2::new(offset.x + 2.0, offset.y + mid_top), Vector2::new(1.0, mid_bot - mid_top + 1.0), Vector4::new(0.0, 0.0, 0.0, 1.0));
                draw_quad(draw, Vector2::new(offset.x, offset.y + mid), Vector2::new(2.0, 1.0), Vector4::new(0.0, 0.0, 0.0, 1.0));
                draw_quad(draw, Vector2::new(offset.x + 3.0, offset.y + mid_top), Vector2::new(2.0, 1.0), Vector4::new(0.0, 0.0, 0.0, 1.0));
                draw_quad(draw, Vector2::new(offset.x + 3.0, offset.y + mid_bot), Vector2::new(2.0, 1.0), Vector4::new(0.0, 0.0, 0.0, 1.0));

                top.draw(Vector2::new(offset.x + 5.0, offset.y), draw);
                bottom.draw(Vector2::new(offset.x + 5.0, offset.y + top.size.y), draw);
            },
            Layout::Vertical(ref top, ref bottom, align) => {
                top.draw(Vector2::new(offset.x + ((self.size.x - top.size.x) * align).floor(), offset.y), draw);
                bottom.draw(Vector2::new(offset.x + ((self.size.x - bottom.size.x) * align).floor(), offset.y + top.size.y), draw);
            },
            Layout::Background(ref child, color) => {
                let size = child.size;
                draw.draw_quad(offset + size / 2.0, size, color);
                child.draw(offset, draw);
            },
            Layout::Border(ref child, dim, color) => {
                let size = child.size;

                if color.w > 0.0 {
                    let vs = Vector2::new(dim.x, size.y);
                    // Left border
                    draw.draw_quad(offset + Vector2::new(0.0, dim.y) + vs / 2.0, vs, color);
                    // Right border
                    draw.draw_quad(offset + Vector2::new(size.x + dim.x, dim.y) + vs / 2.0, vs, color);

                    let hs = Vector2::new(size.x + dim.x * 2.0, dim.y);
                    // Bottom border
                    draw.draw_quad(offset + hs / 2.0, hs, color);
                    // Top Border
                    draw.draw_quad(offset + Vector2::new(0.0, size.y + dim.y) + hs / 2.0, hs, color);
                }

                child.draw(offset + dim, draw);
            },
            Layout::Text(ref s, color) => {
                // draw.draw_quad(offset + self.size / 2.0, self.size, render::color(0xffff00ff));
                let bbox = text_bounds(s);
                // This is a confusing line because bbox is in y-down space and the rest of this is in y-up
                // What this line does is apply an additional offset so that all the glyphs are entirely contained within the [offset, size] box
                // Since the glyphs were generated from start point (0, 0), letters than hang down will make the bbox.max.y > 0
                draw.draw_glyphs(offset + Vector2::new(-bbox.min.x as f32, bbox.max.y as f32), s, color);
                // draw.draw_string(offset + Vector2::new(4.0, 4.5), color, s);
            },
            Layout::Empty(_size) => (),
            Layout::Table(ref entries) => {
                let dimensions = table_dimensions(entries);
                let mut p = offset;
                for (y, row) in entries.iter().enumerate() {
                    p.x = offset.x;
                    for (x, entry) in row.iter().enumerate() {
                        let cell_size = Vector2::new(dimensions.col_widths[x], dimensions.row_heights[y]);
                        let cell_offset = Vector2::new(((cell_size.x - entry.size.x) / 2.0).floor(), ((cell_size.y - entry.size.y) / 2.0).floor());
                        entry.draw(p + cell_offset, draw);
                        p.x += dimensions.col_widths[x];
                    }
                    p.y += dimensions.row_heights[y];
                }
            },
            Layout::NavLeft(ref inner) | Layout::NavRight(ref inner) | Layout::NavCompound(ref inner) => {
                inner.draw(offset, draw);
            },
        }
    }
    pub fn get_highlight_rects(&self, offset: Vector2<f32>, target_path: &Rc<LayoutPath>, results: &mut Vec<(Vector2<f32>, Vector2<f32>)>) {
        if let LayoutPath::End = **target_path {
            let size = self.size;
            results.push((offset, size));
        }
        match *self.layout {
            Layout::Horizontal(ref left, ref right) => {
                left.get_highlight_rects(Vector2::new(offset.x, offset.y + ((self.size.y - left.size.y) / 2.0).floor()), target_path, results);
                right.get_highlight_rects(Vector2::new(offset.x + left.size.x, offset.y + ((self.size.y - right.size.y) / 2.0).floor()), target_path, results);
            },
            Layout::DbgTreeHorizontal(ref left, ref right) => {
                left.get_highlight_rects(Vector2::new(offset.x, offset.y + 5.0), target_path, results);
                right.get_highlight_rects(Vector2::new(offset.x + left.size.x, offset.y + 5.0), target_path, results);
            },
            Layout::DbgTreeVertical(ref top, ref bottom) => {
                top.get_highlight_rects(Vector2::new(offset.x + 5.0, offset.y), target_path, results);
                bottom.get_highlight_rects(Vector2::new(offset.x + 5.0, offset.y + top.size.y), target_path, results);
            },
            Layout::Vertical(ref top, ref bottom, align) => {
                top.get_highlight_rects(Vector2::new(offset.x + ((self.size.x - top.size.x) * align).floor(), offset.y), target_path, results);
                bottom.get_highlight_rects(Vector2::new(offset.x + ((self.size.x - bottom.size.x) * align).floor(), offset.y + top.size.y), target_path, results);
            },
            Layout::Background(ref child, _) => {
                child.get_highlight_rects(offset, target_path, results)
            },
            Layout::Border(ref child, dim, _) => {
                child.get_highlight_rects(offset + dim, target_path, results)
            },
            Layout::Text(_, _) | Layout::Empty(_) => (),
            Layout::Table(ref entries) => {
                let dimensions = table_dimensions(entries);
                let mut p = offset;
                for (y, row) in entries.iter().enumerate() {
                    p.x = offset.x;
                    for (x, entry) in row.iter().enumerate() {
                        let cell_size = Vector2::new(dimensions.col_widths[x], dimensions.row_heights[y]);
                        let cell_offset = Vector2::new(((cell_size.x - entry.size.x) / 2.0).floor(), ((cell_size.y - entry.size.y) / 2.0).floor());
                        entry.get_highlight_rects(p + cell_offset, target_path, results);
                        p.x += dimensions.col_widths[x];
                    }
                    p.y += dimensions.row_heights[y];
                }
            },
            Layout::NavLeft(ref inner) => if let LayoutPath::Left(ref inner_path) = **target_path {
                inner.get_highlight_rects(offset, inner_path, results)
            },
            Layout::NavRight(ref inner) => if let LayoutPath::Right(ref inner_path) = **target_path {
                inner.get_highlight_rects(offset, inner_path, results)
            },
            Layout::NavCompound(ref inner) => if let LayoutPath::Value(ref inner_path) = **target_path {
                inner.get_highlight_rects(offset, inner_path, results)
            },
        }
    }
    pub fn hit(&self, offset: Vector2<f32>, p: Vector2<f32>) -> Option<Rc<LayoutPath>> {
        if contains(offset, self.size, p) {
            let here = || { Some(Rc::new(LayoutPath::End)) };
            match *self.layout {
                Layout::Horizontal(ref left, ref right) => {
                    let r1 = left.hit(Vector2::new(offset.x, offset.y + ((self.size.y - left.size.y) / 2.0).floor()), p);
                    let r2 = right.hit(Vector2::new(offset.x + left.size.x, offset.y + ((self.size.y - right.size.y) / 2.0).floor()), p);
                    r1.or(r2).or_else(here)
                },
                Layout::DbgTreeHorizontal(ref left, ref right) => {
                    let r1 = left.hit(Vector2::new(offset.x, offset.y + 5.0), p);
                    let r2 = right.hit(Vector2::new(offset.x + left.size.x, offset.y + 5.0), p);
                    r1.or(r2).or_else(here)
                },
                Layout::DbgTreeVertical(ref top, ref bottom) => {
                    let r1 = top.hit(Vector2::new(offset.x + 5.0, offset.y), p);
                    let r2 = bottom.hit(Vector2::new(offset.x + 5.0, offset.y + top.size.y), p);
                    r1.or(r2).or_else(here)
                },
                Layout::Vertical(ref top, ref bottom, align) => {
                    let r1 = top.hit(Vector2::new(offset.x + ((self.size.x - top.size.x) * align).floor(), offset.y), p);
                    let r2 = bottom.hit(Vector2::new(offset.x + ((self.size.x - bottom.size.x) * align).floor(), offset.y + top.size.y), p);
                    r1.or(r2).or_else(here)
                },
                Layout::Background(ref child, _) => {
                    child.hit(offset, p).or_else(here)
                },
                Layout::Border(ref child, dim, _) => {
                    child.hit(offset + dim, p).or_else(here)
                },
                Layout::Text(_, _) | Layout::Empty(_) => here(),
                Layout::Table(ref entries) => {
                    let dimensions = table_dimensions(entries);
                    let hit_point = p;
                    let mut p = offset;
                    for (y, row) in entries.iter().enumerate() {
                        p.x = offset.x;
                        for (x, entry) in row.iter().enumerate() {
                            let cell_size = Vector2::new(dimensions.col_widths[x], dimensions.row_heights[y]);
                            let cell_offset = Vector2::new(((cell_size.x - entry.size.x) / 2.0).floor(), ((cell_size.y - entry.size.y) / 2.0).floor());
                            match entry.hit(p + cell_offset, hit_point) {
                                some@Some(_) => { return some; },
                                None => (),
                            }
                            p.x += dimensions.col_widths[x];
                        }
                        p.y += dimensions.row_heights[y];
                    }
                    here()
                },
                Layout::NavLeft(ref inner) => {
                    inner.hit(offset, p).map(|p| Rc::new(LayoutPath::Left(p))).or_else(here)
                },
                Layout::NavRight(ref inner) => {
                    inner.hit(offset, p).map(|p| Rc::new(LayoutPath::Right(p))).or_else(here)
                },
                Layout::NavCompound(ref inner) => {
                    inner.hit(offset, p).map(|p| Rc::new(LayoutPath::Value(p))).or_else(here)
                },
            }
        } else {
            None
        }
    }
}