/*
    stplugin.c, version 1.0
    copyright (c) 2003        			Stata Corp.
*/

#include "stplugin.h"

ST_plugin *_stata_ ;

STDLL pginit(ST_plugin *p)
{
	_stata_ = p ;
	return(SD_PLUGINVER) ;
}


