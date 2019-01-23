#pragma once

/*
* VisionDownsampling.cpp: OpenCV downsampling implementation for HackflightSim vision
*
* Copyright (C) 2017 Simon D. Levy
*
* MIT License
*/

#include "VisionDownsampling.h"

#ifdef _OPENCV

#include <opencv2/video/tracking.hpp>


VisionDownsampling::VisionDownsampling(class AVisionHUD * hud, int leftx, int topy) : VisionAlgorithm(hud, leftx, topy)
{
}

VisionDownsampling::~VisionDownsampling()
{
}

void VisionDownsampling::perform(cv::Mat & bgrimg)
{
    // Convert color image to grayscale
    cv::Mat gray;
    cv::cvtColor(bgrimg, gray, cv::COLOR_BGR2GRAY);

    // Downsmaple (resize)
    int smallrows = gray.rows / DOWNSAMPLE_RATIO;
    int smallcols = gray.cols / DOWNSAMPLE_RATIO;
	cv::Mat smallgray(smallrows, smallcols, CV_8UC1);
    cv::resize(gray, smallgray, cv::Size(smallcols,smallrows), 0, 0, CV_INTER_LINEAR);

    // Start new image display at a horizontal offset from original
    int startx = _leftx + gray.cols + IMAGE_MARGIN; 

    // Draw pixels to HUD
    for (int r=0; r<smallrows; ++r) {
        for (int c=0; c<smallcols; ++c) {
            int x = startx + c * DOWNSAMPLE_RATIO;
            int y = _topy  + r * DOWNSAMPLE_RATIO;
            uint8_t grayval = smallgray.at<uint8_t>(r,c);
	        FColor color(grayval,grayval,grayval,255); // alpha=255 (opaque)
            _hud->DrawRect(color, x, y, DOWNSAMPLE_RATIO, DOWNSAMPLE_RATIO);
        }
    }

    // Draw a border around the pixels
	_hud->drawBorder(startx - IMAGE_MARGIN);
}
#endif