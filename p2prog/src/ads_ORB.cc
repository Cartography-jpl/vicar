// ads_ORB.h
//
//  April 9, 2002
//  Michael Brady

#include "ads_ORB.h"


static CORBA::ORB_var g_orb = CORBA::ORB::_nil();

void ads_ORB::init(int argc, char** argv)
{
    if (CORBA::is_nil(g_orb.in()))
	{
	    g_orb = CORBA::ORB_init (argc, argv, 0);
	}
}

   
CORBA::ORB_ptr ads_ORB::instance()
throw ()
{
    return g_orb.in();
}

