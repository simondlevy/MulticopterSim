/*
   DVS simulator demo program

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include <opencv2/highgui/highgui.hpp>

#include "Davis346Sim.hpp"

// meters, seconds
static constexpr double TARGET_SIZE     = 1; 
static constexpr double TARGET_DISTANCE = 10; 
static constexpr double TARGET_SPEED    = 0.5; 

int main(int argc, char ** argv)
{
    Davis346 dvs(TARGET_SIZE);

    while (true) {

        cv::Mat cameraImage = cv::Mat::zeros(Davis346::RES_ROWS, Davis346::RES_COLS, CV_8UC3);

        cv::imshow("EventCamera", cameraImage);

        if (cv::waitKey(1) == 27) break; // quit on ESC
    }

    return 0;
}


