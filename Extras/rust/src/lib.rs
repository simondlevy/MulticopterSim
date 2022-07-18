#[repr(C)]
pub struct Motors {
    m1: f32,
    m2: f32,
    m3: f32,
    m4: f32
}

#[no_mangle]
pub extern "C" fn get_motors() -> Motors {
	Motors { m1: 1.0, m2: 2.0, m3: 3.0, m4:4.0 }
}
