// ads_SpiceLib_i_2.cc
//
// This file contains all the parts of SpiceLib which are not SPICE toolkit
// wrappers.

#include "ads_SpiceLib_i.h"
#include "ads_ORB.h"
#include "adc_Instrument.h"
#include "ads_TraceAdder.h"

#include <string>
using std::string;

#include "SpiceUsr.h"

///////////////////////////////////////////////////////////
// Static local funcions
///////////////////////////////////////////////////////////

static void initFinder(auto_ptr<ads_MissionDataFinder>& dataFinder,
                 jpl::mipl::spice::corba::Instrument inst)
{
   ADS_TRACE;

   if (dataFinder.get() == 0)
   {
      dataFinder.reset(ads_MissionDataFinder::createDataFinder(inst));
   }
}

///////////////////////////////////////////////////////////
// Non-SPICE toolkit methods
///////////////////////////////////////////////////////////


void 
ads_SpiceLib_i::getGeometry (
	const jpl::mipl::spice::corba::ImageData & image,
   const jpl::mipl::spice::corba::GeometryMetaData & criteria,
	jpl::mipl::spice::corba::Geometry_out theGeometry,
	jpl::mipl::spice::corba::GeometryMetaData_out metaData,
	CORBA::Environment & )
	throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException,
           jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound )
{
   ADS_TRACE;

   initFinder(m_dataFinder, image.inst);

   metaData = new jpl::mipl::spice::corba::GeometryMetaData;

   m_dataFinder->getGeometry(image,
                             criteria,
                             theGeometry,
                             metaData);
}


void 
ads_SpiceLib_i::
storeGeometry (
      const jpl::mipl::spice::corba::ImageData & image,
      const jpl::mipl::spice::corba::Geometry & theGeometry,
      const jpl::mipl::spice::corba::GeometryMetaData & theMetaData,
      CORBA::Environment & )
   throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   ADS_TRACE;

   initFinder(m_dataFinder, image.inst);

   m_dataFinder->storeGeometry(image, theGeometry, theMetaData);
   
}
          
void ads_SpiceLib_i::getInstrument (
	 const jpl::mipl::spice::corba::ImageData & image,
	 jpl::mipl::spice::corba::InstrumentData_out theInstrument,
	 CORBA::Environment & )
   throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   ADS_TRACE;

   initFinder(m_dataFinder, image.inst);

   m_dataFinder->getInstrument(image, theInstrument);
}

void ads_SpiceLib_i::getTarget (
     	const jpl::mipl::spice::corba::ImageData & image,
      jpl::mipl::spice::corba::Target_out theTarget,
      CORBA::Environment & )
	throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   ADS_TRACE;

   initFinder(m_dataFinder, image.inst);

   m_dataFinder.get()->getTarget(image, theTarget);
}
