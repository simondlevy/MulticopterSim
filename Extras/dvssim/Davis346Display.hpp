/*
   OpenCV-based display for iniVation DAVIS346 Dynamic Vision Sensor simulation

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include <opencv2/highgui/highgui.hpp>

class Davis346Display {

    private:

        cv::Mat _cameraImage = cv::Mat::zeros(Davis346::RES_ROWS, Davis346::RES_COLS, CV_8UC3);

    public:

        Davis346Display(void)
        {
        }

        ~Davis346Display(void)
        {
        }

        bool displayEvents(void)
        {
            cv::imshow("EventCamera", _cameraImage);

            return cv::waitKey(1) != 27; // ESC
        }

}; 
