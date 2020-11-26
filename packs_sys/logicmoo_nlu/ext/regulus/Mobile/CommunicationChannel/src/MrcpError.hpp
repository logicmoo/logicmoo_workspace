/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpError.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef MRCPERROR_HPP_
#define MRCPERROR_HPP_

void mrcp_information(char const *logging_tag, 
						char const *calling_function,
						char const *fmt, ...);
		      
#endif /*MRCPERROR_HPP_*/
