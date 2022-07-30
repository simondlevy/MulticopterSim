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

    fn _write_double(val:f32, mut buf:[u8; OUT_BUF_SIZE], idx:usize) {
        let src = (val as f64).to_le_bytes();
        let beg = 8 * idx;
        buf[beg+0] = src[0];
        buf[beg+1] = src[1];
        buf[beg+2] = src[2];
        buf[beg+3] = src[3];
        buf[beg+4] = src[4];
        buf[beg+5] = src[5];
        buf[beg+6] = src[6];
        buf[beg+7] = src[7];
    }

    //let motor_client_socket = UdpSocket::bind("0.0.0.1:5000")?;
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

        let src = (0.6 as f64).to_le_bytes();
        out_buf[0] = src[0];
        out_buf[1] = src[1];
        out_buf[2] = src[2];
        out_buf[3] = src[3];
        out_buf[4] = src[4];
        out_buf[5] = src[5];
        out_buf[6] = src[6];
        out_buf[7] = src[7];

         //write_double(0.6, out_buf, 0);
        //write_double(0.6, out_buf, 1);
        //write_double(0.6, out_buf, 2);
        //write_double(0.6, out_buf, 3);

        motor_client_socket.send_to(&out_buf, "127.0.0.1:5000")?;
    }
}
