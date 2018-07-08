#pragma once

/*
* VisionDownsampling.h: Header for OpenCV downsampling implementation
*
* Copyright (C) 2017 Simon D. Levy
*
* MIT License
*/

#include "VisionAlgorithm.h"

class VisionDownsampling : public VisionAlgorithm {

public:

    VisionDownsampling(class AVisionHUD*hud, int leftx, int topy);

    ~VisionDownsampling();

	virtual void perform(cv::Mat & bgrimg) override;

private:

	const int DOWNSAMPLE_RATIO = 16;

    const int IMAGE_MARGIN = 20;
};
