#[repr(C)]
pub struct Vec2 {
    x: f32,
    y: f32,

    // Adding a constructor here makes the code crash on Windows! :-O
    // Vec2(f32 x_, f32 y_) : x(x_), y(y_) {}
}

#[no_mangle]
pub extern "C" fn vec2_init() -> Vec2 {
	Vec2 { x: 3.0, y: 5.0 }
}
