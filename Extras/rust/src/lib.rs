#[repr(C)]
pub struct Motors {
    m1: f32,
    m2: f32,
    m3: f32,
    m4: f32
}

#[no_mangle]
pub extern "C" fn get_motors() -> Motors {
	Motors { m1: 0.6, m2: 0.6, m3: 0.6, m4:0.6 }
}
