// ads_MissionDataFinder.cc
//
//  April 9, 2002
//  Michael Brady

#include "ads_MissionDataFinder.h"
#include "ads_DefaultDataFinder.h"
#include "ads_CassiniDataFinder.h"
#include "ads_TraceAdder.h"

using std::auto_ptr;

ads_MissionDataFinder::~ads_MissionDataFinder()
{
}

ads_MissionDataFinder*
ads_MissionDataFinder::
createDataFinder(jpl::mipl::spice::corba::Instrument inst)
{
   ADS_TRACE;

   switch (inst)
   {
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_FULL:
      return new ads_CassiniDataFinder;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM22:
      return new ads_CassiniDataFinder;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM44:
      return new ads_CassiniDataFinder;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_FULL:
      return new ads_CassiniDataFinder;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM22:
      return new ads_CassiniDataFinder;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM44:
      return new ads_CassiniDataFinder;
   case jpl::mipl::spice::corba::GALILEO_SSI_FULL:
      return new ads_DefaultDataFinder;
   case jpl::mipl::spice::corba::GALILEO_SSI_SUM:
      return new ads_DefaultDataFinder;
   case jpl::mipl::spice::corba::VGR_1_ISSNA:
      return new ads_DefaultDataFinder;
   case jpl::mipl::spice::corba::VGR_1_ISSWA:
      return new ads_DefaultDataFinder;
   case jpl::mipl::spice::corba::VGR_2_ISSNA:
      return new ads_DefaultDataFinder;
   case jpl::mipl::spice::corba::VGR_2_ISSWA:
      return new ads_DefaultDataFinder;

   default:
      return 0;
   }
}
