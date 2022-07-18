#[repr(C)]
pub struct Vec2 {
    x: f32,
    y: f32,

    // Adding a constructor here makes the code crash on Windows! :-O
    // Vec2(f32 x_, f32 y_) : x(x_), y(y_) {}
}

#[repr(C)]
pub struct Vec4 {
    x: f32,
    y: f32,
    z: f32,
    w: f32,

    // Adding a constructor here is no problem.
    // Vec4(f32 x_, f32 y_, f32 z_, f32 w_) : x(x_), y(y_), z(z_), w(w_) {}
}

#[no_mangle]
pub extern "C" fn vec2_init() -> Vec2 {
	Vec2 { x: 3.0, y: 5.0 }
}


#[no_mangle]
pub extern "C" fn vec4_init() -> Vec4 {
	Vec4 { x: 3.0, y: 5.0, z: 7.0, w: 9.0 }
}
