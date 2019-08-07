/*
   DVS simulator demo program

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include <opencv2/highgui/highgui.hpp>

#include "Davis346Sim.hpp"

static const uint16_t CAMERA_COLS = 346;
static const uint16_t CAMERA_ROWS = 260;

// meters
static constexpr double TARGET_SIZE = 1; 
static constexpr double TARGET_DIST = 10; 

int main(int argc, char ** argv)
{
    Davis346 dvs(TARGET_SIZE);

    while (true) {

        cv::Mat cameraImage = cv::Mat::zeros(CAMERA_ROWS, CAMERA_COLS, CV_8UC3);

        cv::imshow("EventCamera", cameraImage);

        if (cv::waitKey(1) == 27) break; // quit on ESC
    }

    return 0;
}


