/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	GlobalDefs.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef GLOBALDEFS_HPP_
#define GLOBALDEFS_HPP_

#define APP_NAME "MedSLT Doctor GUI"
#define APP_VERSION "1.0"

#ifndef MEDSLTDOC_DATA_DIR
//#define MEDSLTDOC_DATA_DIR "/home/tsouraki/workspace/MaemoISSCO/MedSLTDoctorGUI/data/pixmaps"
#define MEDSLTDOC_DATA_DIR "/home/user/MedSLT/data/pixmaps"
#endif

static const int color_map[4][4] = {{225*256, 225*256, 225*256}, 
									{200*256, 200*256, 200*256}, 
									{175*256, 175*256, 175*256},
									{255*256, 248*256, 220*256}};
#endif /*GLOBALDEFS_HPP_*/
