use std::net::UdpSocket;

//use datatypes::datatypes::AltHoldPid;
//use datatypes::datatypes::Demands;
//use datatypes::datatypes::VehicleState;

//use hackflight::hackflight::run_hackflight2;

//pub mod alt_hold;
//pub mod datatypes;
//pub mod hackflight;

fn main() -> std::io::Result<()> {

    const IN_BUF_SIZE:usize  = 17*8; // 17 doubles in
    const OUT_BUF_SIZE:usize = 4*8;  // 4 doubles out

    fn read_float(buf:[u8; IN_BUF_SIZE], idx:usize) -> f32 {
        let mut dst = [0u8; 8];
        let beg = 8 * idx;
        let end = beg + 8;
        dst.clone_from_slice(&buf[beg..end]);
        f64::from_le_bytes(dst) as f32
    }

    let motor_client_socket = UdpSocket::bind("0.0.0.0:0")?;
    let telemetry_server_socket = UdpSocket::bind("127.0.0.1:5001")?;

    /*
    let mut alt_hold_pid = AltHoldPid {
        error_integral: 0.0,
        in_band: false,
        target: 0.0
    };
    */

    println!("Hit the Play button ...");

    loop {

        let mut in_buf = [0; IN_BUF_SIZE]; 
        telemetry_server_socket.recv_from(&mut in_buf)?;

        let time = read_float(in_buf, 0);

        if time < 0.0 { break Ok(()); }

        println!("{}", time);

        /*
        let vehicle_state = VehicleState {
            x:read_float(in_buf, 1),
            dx:read_float(in_buf, 2),
            y:read_float(in_buf, 3),
            dy:read_float(in_buf, 4),
            z:read_float(in_buf, 5),
            dz:read_float(in_buf, 6),
            phi:read_float(in_buf, 7),
            dphi:read_float(in_buf, 8),
            theta:read_float(in_buf, 9),
            dtheta:read_float(in_buf, 10),
            psi:read_float(in_buf, 11),
            dpsi:read_float(in_buf, 12)
        };

        let demands = Demands {
            throttle:read_float(in_buf, 13),
            roll:read_float(in_buf, 14),
            pitch:read_float(in_buf, 15),
            yaw:read_float(in_buf, 16)
        };

        
        let (new_alt_hold_pid, _motors) =
            run_hackflight2(demands, vehicle_state, alt_hold_pid.clone());

        // alt_hold_pid.error_integral = new_alt_hold_pid.error_integral;
        alt_hold_pid = new_alt_hold_pid;
        */

        let mut out_buf = [0u8; OUT_BUF_SIZE]; 

        let motors = [0.1, 0.2, 0.3, 0.4];

        for j in 0..4 {

            let bytes = (motors[j] as f64).to_le_bytes();

            for k in 0..8 {
                out_buf[j*8+k] = bytes[k];
            }
        }

        motor_client_socket.send_to(&out_buf, "127.0.0.1:5000")?;
    }
}
