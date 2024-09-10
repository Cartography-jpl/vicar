// ads_SpiceLib_i.cc
//
// This file contains all the parts of SpiceLib which are SPICE toolkit
// wrappers.

#include "ads_SpiceLib_i.h"
#include "ads_ORB.h"
#include "ads_TraceAdder.h"
#include "ads_error.h"

#include <string>
using std::string;

#include "SpiceUsr.h"


////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////


static void Spice2CorbaEllipse (const SpiceEllipse& spice, 
			 jpl::mipl::spice::corba::SpEllipse& corba)
{
   memcpy(corba.center, spice.center, sizeof(corba.center));
   memcpy(corba.semiMajor, spice.semiMajor, sizeof(corba.semiMajor));
   memcpy(corba.semiMinor, spice.semiMinor, sizeof(corba.semiMinor));
}

static void Corba2SpiceEllipse (const jpl::mipl::spice::corba::SpEllipse& corba,
			 SpiceEllipse& spice )
{
   memcpy(spice.center, corba.center, sizeof(spice.center));
   memcpy(spice.semiMajor, corba.semiMajor, sizeof(spice.semiMajor));
   memcpy(spice.semiMinor, corba.semiMinor, sizeof(spice.semiMinor));
}

static void Spice2CorbaPlane (const SpicePlane& spice, 
			 jpl::mipl::spice::corba::SpPlane& corba)
{
   memcpy(corba.normal, spice.normal, sizeof(corba.normal));
   corba.constant = spice.constant;
}

static void Corba2SpicePlane (const jpl::mipl::spice::corba::SpPlane& corba,
			 SpicePlane& spice )
{
   memcpy(spice.normal, corba.normal, sizeof(spice.normal));
   spice.constant = corba.constant;
}

static 
jpl::mipl::spice::corba::EKDataType Spice2CorbaEKType(SpiceEKDataType type)
{
   switch (type)
   {
      case SPICE_CHR:   return jpl::mipl::spice::corba::SPICE_CHR;
      case SPICE_DP:    return jpl::mipl::spice::corba::SPICE_DP;
      case SPICE_INT:   return jpl::mipl::spice::corba::SPICE_INT;
      case SPICE_TIME:  return jpl::mipl::spice::corba::SPICE_TIME;
      default:
	 // TODO handle this error.
	 return jpl::mipl::spice::corba::SPICE_CHR;
   }
}

static
jpl::mipl::spice::corba::EKExprClass Spice2CorbaEKClass(SpiceEKExprClass type)
{
   switch (type)
   {
      case SPICE_EK_EXP_COL:   return jpl::mipl::spice::corba::SPICE_EK_EXP_COL;
      case SPICE_EK_EXP_FUNC:    return jpl::mipl::spice::corba::SPICE_EK_EXP_FUNC;
      case SPICE_EK_EXP_EXPR:   return jpl::mipl::spice::corba::SPICE_EK_EXP_EXPR;
      default:
	 // TODO handle this error
	 return jpl::mipl::spice::corba::SPICE_EK_EXP_COL;
   }
}


static void Spice2CorbaEKAttDsc ( const SpiceEKAttDsc& src,
				  jpl::mipl::spice::corba::EKAttDsc& dest )
{
    dest.cclass = src.cclass;
    dest.dtype  = Spice2CorbaEKType(src.dtype);
    dest.strlen = src.strlen;
    dest.size   = src.size;
    dest.indexd = src.indexd;
    dest.nullok = src.nullok;
}





////////////////////////////////////////////////////////
// Free-standing functions used only by SpiceLib_i
////////////////////////////////////////////////////////

/**
 *  Makes sure that SpiceInt (a long) is the same size as
 *  CORBA::Long (an int).  These bridges assume that this is the case.
 * 
 */
static void sanityCheck()
{
   if (sizeof(SpiceInt) != sizeof(jpl::mipl::spice::corba::SpInt))
   {
       cerr << "Error.  This program may only be run on platforms "
	   "where a C++ int is the same size as a C++ long." << endl;
       exit(4);
   }

   if (sizeof(SpiceChar) != sizeof(char))
   {
       cerr << "Error.  This program may only be run on platforms "
	   "where a C++ char is the same size as a SpiceChar." << endl;
       exit(5);
   }

}

static void setErrorAction()
{
   char action[64];
   memset(action, '\0', sizeof(action));
   strcpy(action, "RETURN");

   erract_c ("SET", sizeof(action), action);
}


static void initToolkit()
{
   sanityCheck();
   setErrorAction();
}


////////////////////////////////////////////////////////
// SpiceLib_i implementation
////////////////////////////////////////////////////////


ads_SpiceLib_i::ads_SpiceLib_i ()
   :
   SpiceLib(),
   m_poa(PortableServer::POA::_nil()),
   m_id(-1)
{
   initToolkit();
}



ads_SpiceLib_i::ads_SpiceLib_i (const ads_SpiceLib_i& rhs)
   :
   SpiceLib(rhs),
   m_poa(rhs.m_poa),
   m_id(-1)
{
   initToolkit();
}

ads_SpiceLib_i::ads_SpiceLib_i (PortableServer::POA_ptr poa)
   :
   SpiceLib(),
   m_poa(poa),
   m_id(-1)
{
   initToolkit();
}

ads_SpiceLib_i::~ads_SpiceLib_i ()
{
}



PortableServer::POA_ptr 
ads_SpiceLib_i::_default_POA()
{
   return ( (! CORBA::is_nil(m_poa)) ? m_poa :
	   PortableServer::ServantBase::_default_POA());
}

///////////////////////////////////////////////////////////
// Non-SPICE toolkit methods
///////////////////////////////////////////////////////////


void 
ads_SpiceLib_i::
init (jpl::mipl::spice::corba::SpiceLibFactory_ptr factory,
      CORBA::Long id,
      const char * loadFilesHint,
      CORBA::Environment &
      )
   throw (jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   m_factory = jpl::mipl::spice::corba::SpiceLibFactory::_duplicate(factory);
   m_id = id;
   loadFiles(loadFilesHint);
}

        void 
ads_SpiceLib_i::loadFiles (
            const char * hint,
            CORBA::Environment& 
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   ACE_TRACE ("ads_SpiceLib_i::loadFiles");

   string loadHint;
   string oldHint = ((m_loadFilesHint.in() == 0) ? 
                     "" : m_loadFilesHint.in());

   // For now, just slap a .furnsh on the load hint to derive a file name.

   loadHint = hint;
   loadHint += ".furnsh";

   if (oldHint.size() > 0)
   {
      oldHint  += ".furnsh";
      unload_c(oldHint.c_str());
      ads_checkForError();
   }

   furnsh_c(loadHint.c_str());
   ads_checkForError();
   m_loadFilesHint = CORBA::string_dup(hint);
}


CORBA::Long 
ads_SpiceLib_i::id (CORBA::Environment &)
   throw ()
{
   return m_id;
}
          
void ads_SpiceLib_i::shutdown (
   CORBA::Environment& 
   )
  throw ()
{
   ads_ORB::instance()->shutdown();
}

char * 
ads_SpiceLib_i::loadFilesHint(CORBA::Environment &)
   throw ()
{
   return CORBA::string_dup(m_loadFilesHint.in());
}

void 
ads_SpiceLib_i::close (CORBA::Environment &)
      throw ()
{
   if (!CORBA::is_nil(m_factory.in()))
   {
      m_factory->closed(_this());
   }
}

///////////////////////////////////////////////////////////
// SPICE toolkit methods
///////////////////////////////////////////////////////////

        void 
ads_SpiceLib_i::axisar (
            const jpl::mipl::spice::corba::Vector3 axis,
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::Matrix33_out r,
            CORBA::Environment& 
          )
          throw ()
{
   axisar_c(axis, angle, r);
}


        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::b1900 (
   CORBA::Environment& 
	   )
          throw ()
{
   return b1900_c ();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::b1950 (
            CORBA::Environment& 
          )
          throw ()
{
   return b1950_c ();
}

        void 
ads_SpiceLib_i::bodc2n (
            jpl::mipl::spice::corba::SpInt code,
            CORBA::String_out name,
            CORBA::Boolean_out found,
            CORBA::Environment& 
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const SpiceInt MAX_LENGTH = 33;
   SpiceChar buf[MAX_LENGTH];
   memset(buf, '\0', MAX_LENGTH);

   SpiceBoolean tFound = SPICEFALSE;
   bodc2n_c (code, MAX_LENGTH, buf, &tFound);
   found  = static_cast<CORBA::Boolean>(tFound);

   name = CORBA::string_dup(buf);

}

        void 
ads_SpiceLib_i::boddef (
            const char * name,
            jpl::mipl::spice::corba::SpInt code,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   boddef_c(name, code);
   ads_checkForError();
}


        CORBA::Boolean 
ads_SpiceLib_i::bodfnd (
            jpl::mipl::spice::corba::SpInt body,
            const char * item,
            CORBA::Environment &
          )
          throw ()
{
   SpiceBoolean ret = bodfnd_c(body, item);
   ads_checkForError();
   
   return static_cast<CORBA::Boolean>(ret);
}

        void 
ads_SpiceLib_i::bodn2c (
            const char * name,
            jpl::mipl::spice::corba::SpInt_out code,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ()
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tCode = 0;
   bodn2c_c(name, &tCode, &tFound);
   ads_checkForError();
   found = static_cast<CORBA::Boolean>(tFound);
   code = static_cast<jpl::mipl::spice::corba::SpInt>(tCode);
}


        void 
ads_SpiceLib_i::bodvar (
            jpl::mipl::spice::corba::SpInt body,
            const char * item,
            jpl::mipl::spice::corba::SpInt_out dim,
            jpl::mipl::spice::corba::SpDouble8_out values,
            CORBA::Environment &
          )
          throw ()
{
   SpiceInt tDim = 0;

   bodvar_c(body, item, &tDim, values);

   dim = static_cast<jpl::mipl::spice::corba::SpInt>(tDim);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::cgv2el (
            const jpl::mipl::spice::corba::Vector3 center,
            const jpl::mipl::spice::corba::Vector3 vec1,
            const jpl::mipl::spice::corba::Vector3 vec2,
            jpl::mipl::spice::corba::SpEllipse_out ellipse,
            CORBA::Environment &
          )
          throw ()
{
   SpiceEllipse tEllipse;
   memset(&tEllipse, 0, sizeof(tEllipse));

   cgv2el_c(center, vec1, vec2, &tEllipse);

   Spice2CorbaEllipse(tEllipse, ellipse);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::chkin (
            const char * moduleName,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   chkin_c(moduleName);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::chkout (
            const char * moduleName,
            CORBA::Environment& 
          )
          throw ()
{
   chkout_c(moduleName);
}

        void 
ads_SpiceLib_i::cidfrm (
            jpl::mipl::spice::corba::SpInt cent,
            jpl::mipl::spice::corba::SpInt_out frcode,
            CORBA::String_out frname,
            CORBA::Boolean_out found,
            CORBA::Environment& 
          )
          throw ()
{
   char tFrname[64];
   memset(tFrname, '\0', sizeof(tFrname));
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tFrcode = 0;
   cidfrm_c(cent, sizeof(tFrname), &tFrcode, tFrname, &tFound);

   frname = CORBA::string_dup(tFrname);
   found = static_cast<CORBA::Boolean>(tFound);
   frcode = static_cast<jpl::mipl::spice::corba::SpInt>(tFrcode);
}

        void 
ads_SpiceLib_i::ckgp (
            jpl::mipl::spice::corba::SpInt inst,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            jpl::mipl::spice::corba::SpDouble tol,
            const char * ref,
            jpl::mipl::spice::corba::Matrix33_out cmat,
            jpl::mipl::spice::corba::SpDouble_out clkout,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   
   ckgp_c(inst, sclkdp, tol, ref, cmat, &clkout, &tFound);

   found = static_cast<CORBA::Boolean>(tFound);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::ckgpav (
            jpl::mipl::spice::corba::SpInt inst,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            jpl::mipl::spice::corba::SpDouble tol,
            const char * ref,
            jpl::mipl::spice::corba::Matrix33_out cmat,
            jpl::mipl::spice::corba::Vector3_out av,
            jpl::mipl::spice::corba::SpDouble_out clkout,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;

   ckgpav_c(inst, sclkdp, tol, ref, cmat, av, &clkout, &tFound);

   found = static_cast<CORBA::Boolean>(tFound);

   ads_checkForError();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::clight (
            CORBA::Environment& 
          )
          throw ()
{
   return clight_c ();
}

        void 
ads_SpiceLib_i::cnmfrm (
            const char * cname,
            jpl::mipl::spice::corba::SpInt_out frcode,
            CORBA::String_out frname,
            CORBA::Boolean_out found,
            CORBA::Environment& 
          )
          throw ()
{
   char tFrname[128];
   memset(tFrname, '\0', sizeof(tFrname));

   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tFrcode = 0;

   cnmfrm_c(cname, sizeof(tFrname), &tFrcode, tFrname, &tFound);

   frname = CORBA::string_dup(tFrname);
   found = static_cast<CORBA::Boolean>(tFound);
   frcode = static_cast<jpl::mipl::spice::corba::SpInt>(tFrcode);
}


        void 
ads_SpiceLib_i::conics (
            const jpl::mipl::spice::corba::SpDouble8 elts,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::State_out theState,
            CORBA::Environment& 
          )
          throw ()
{
   conics_c(elts, et, theState);
}

        void 
ads_SpiceLib_i::convrt (
            jpl::mipl::spice::corba::SpDouble x,
            const char * inStr,
            const char * outStr,
            jpl::mipl::spice::corba::SpDouble_out y,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   convrt_c(x, inStr, outStr, &y);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::cyllat (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble lonc,
            jpl::mipl::spice::corba::SpDouble z,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out lat,
            CORBA::Environment& 
          )
          throw ()
{
   cyllat_c(r, lonc, z, &radius, &lon, &lat);
}

        void 
ads_SpiceLib_i::cylrec (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble z,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment& 
          )
          throw ()
{
   cylrec_c(r, lon, z, rectan);
}

        void 
ads_SpiceLib_i::cylsph (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble lonc,
            jpl::mipl::spice::corba::SpDouble z,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out colat,
            jpl::mipl::spice::corba::SpDouble_out lon,
            CORBA::Environment& 
          )
          throw ()
{
   cylsph_c(r, lonc, z, &radius, &colat, &lon);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::det (
            const jpl::mipl::spice::corba::Matrix33 m1,
            CORBA::Environment& 
          )
          throw ()
{
   return det_c(m1);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::dpmax (
            CORBA::Environment& 
          )
          throw ()
{
   return dpmax_c();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::dpmin (
            CORBA::Environment& 
          )
          throw ()
{
   return dpmin_c();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::dpr (
            CORBA::Environment& 
          )
          throw ()
{
   return dpr_c();
}

        void 
ads_SpiceLib_i::dtpool (
            const char * name,
            CORBA::Boolean_out found,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpChar1_out type,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tN = 0;
   
   dtpool_c(name, &tFound, &tN, type);
   
   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);
   found = static_cast<CORBA::Boolean>(tFound);
   ads_checkForError();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::dvdot (
            const jpl::mipl::spice::corba::State s1,
            const jpl::mipl::spice::corba::State s2,
            CORBA::Environment& 
          )
          throw ()
{
   return dvdot_c(s1, s2);
}

        void 
ads_SpiceLib_i::dvhat (
            const jpl::mipl::spice::corba::State s1,
            jpl::mipl::spice::corba::State_out sout,
            CORBA::Environment& 
          )
          throw ()
{
   dvhat_c(s1, sout);
}

        void 
ads_SpiceLib_i::edlimb (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 viewpt,
            jpl::mipl::spice::corba::SpEllipse_out limb,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceEllipse tLimb;
   memset(&tLimb, 0, sizeof(tLimb));
   
   edlimb_c(a, b, c, viewpt, &tLimb);
   ads_checkForError();

   Spice2CorbaEllipse(tLimb, limb);
}


void ads_SpiceLib_i::ekccnt (
	     const char * table,
	     jpl::mipl::spice::corba::SpInt_out ccount,
	     CORBA::Environment &
	     )
	     throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
    SpiceInt tCcount = 0;

    ekccnt_c ( table, &tCcount );
    ads_checkForError();

    ccount = tCcount;
}
        

void ads_SpiceLib_i::ekcii (
	    const char * table,
	    jpl::mipl::spice::corba::SpInt cindex,
	    CORBA::String_out column,
	    jpl::mipl::spice::corba::EKAttDsc_out attdsc,
	    CORBA::Environment &
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
    const size_t COLUMN_STRING_LENGTH = 64;
    SpiceChar tColumn[COLUMN_STRING_LENGTH];
    memset(tColumn, '\0', sizeof(tColumn));

    SpiceEKAttDsc tAttdsc;
    memset(&tAttdsc, 0, sizeof(tAttdsc));

    ekcii_c(table, cindex, sizeof(tColumn), tColumn, &tAttdsc);
    ads_checkForError();

    column = CORBA::string_dup(tColumn);
    Spice2CorbaEKAttDsc(tAttdsc, attdsc);
}


        void 
ads_SpiceLib_i::ekfind (
            const char * query,
            jpl::mipl::spice::corba::SpInt_out nmrows,
            CORBA::Boolean_out error,
            CORBA::String_out errmsg,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tError = SPICEFALSE;
   SpiceInt tNmrows = 0;

   const int ERRMSG_MAX_LENGTH = 1024;
   SpiceChar buf[ERRMSG_MAX_LENGTH];
   memset(buf, '\0', sizeof(buf));

   ekfind_c(query, sizeof(buf), &tNmrows, &tError, buf);

   errmsg = CORBA::string_dup(buf);
   nmrows = static_cast<jpl::mipl::spice::corba::SpInt>(tNmrows);
   error = static_cast<CORBA::Boolean>(tError);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::ekgc (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            jpl::mipl::spice::corba::SpInt elment,
            CORBA::String_out cdata,
            CORBA::Boolean_out null,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const int ERRMSG_MAX_LENGTH = 1024;
   SpiceChar buf[ERRMSG_MAX_LENGTH];
   memset(buf, '\0', sizeof(buf));

   SpiceBoolean tFound = SPICEFALSE;
   SpiceBoolean tNull = SPICEFALSE;
   ekgc_c(selidx, row, elment, ERRMSG_MAX_LENGTH, buf, &tNull, &tFound);
   cdata = CORBA::string_dup(buf);
   found = static_cast<CORBA::Boolean>(tFound);
   null = static_cast<CORBA::Boolean>(tNull);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::ekgd (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            jpl::mipl::spice::corba::SpInt elment,
            jpl::mipl::spice::corba::SpDouble_out ddata,
            CORBA::Boolean_out null,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceBoolean tNull = SPICEFALSE;
   ekgd_c(selidx, row, elment, &ddata, &tNull, &tFound);
   found = static_cast<CORBA::Boolean>(tFound);
   null = static_cast<CORBA::Boolean>(tNull);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::ekgi (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            jpl::mipl::spice::corba::SpInt elment,
            jpl::mipl::spice::corba::SpInt_out idata,
            CORBA::Boolean_out null,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceBoolean tNull = SPICEFALSE;
   SpiceInt tIdata = 0;
      
   ekgi_c(selidx, row, elment, &tIdata, &tNull, &tFound);
   
   idata = static_cast<jpl::mipl::spice::corba::SpInt>(tIdata);
   found = static_cast<CORBA::Boolean>(tFound);
   null = static_cast<CORBA::Boolean>(tNull);

   ads_checkForError();
}

        jpl::mipl::spice::corba::SpInt 
ads_SpiceLib_i::eknelt (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   jpl::mipl::spice::corba::SpInt ret = eknelt_c(selidx, row);
   ads_checkForError();
   return ret;
}


void 
ads_SpiceLib_i::ekntab (
	     jpl::mipl::spice::corba::SpInt_out n,
	     CORBA::Environment &
            )
	    throw ()
{
    SpiceInt tN = 0;
    
    ekntab_c( &tN );

    n = tN;
}


void 
ads_SpiceLib_i::ekpsel (
   const char * query,
   jpl::mipl::spice::corba::SpInt_out n,
   jpl::mipl::spice::corba::SpIntSeq_out xbegs,
   jpl::mipl::spice::corba::SpIntSeq_out xends,
   jpl::mipl::spice::corba::EKDataTypeSeq_out xtypes,
   jpl::mipl::spice::corba::EKExprClassSeq_out xclass,
   jpl::mipl::spice::corba::stringSeq_out tabs,
   jpl::mipl::spice::corba::stringSeq_out cols,
   CORBA::Boolean_out error,
   CORBA::String_out errmsg,
   CORBA::Environment &
   )
   throw( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tError = SPICEFALSE;
   SpiceInt tN = 0;


   /*
    * SPICE_EK_MAXQSEL, SPICE_EK_CSTRLN, and SPICE_EK_TSTRLN are
    * defined in SpiceEK.h
    */
   const size_t lenTabs = SPICE_EK_MAXQSEL * SPICE_EK_TSTRLN;
   const size_t lenCols = SPICE_EK_MAXQSEL * SPICE_EK_CSTRLN;

   SpiceChar tTabs[lenTabs];
   SpiceChar tCols[lenCols];
   memset(tTabs, '\0', lenTabs);
   memset(tCols, '\0', lenCols);

   const size_t ERR_MSG_LEN = 80;
   SpiceChar tErrMsg[ERR_MSG_LEN];
   memset(tErrMsg, '\0', sizeof(tErrMsg));


   SpiceInt tXbegs[SPICE_EK_MAXQSEL];
   memset(tXbegs, 0, sizeof(tXbegs));
   SpiceInt tXends[SPICE_EK_MAXQSEL];
   memset(tXends, 0, sizeof(tXbegs));
   SpiceEKDataType  tXtypes[SPICE_EK_MAXQSEL];
   memset(tXtypes, 0, sizeof(tXtypes));
   SpiceEKExprClass tXclass[SPICE_EK_MAXQSEL];
   memset(tXclass, 0, sizeof(tXclass));


   ekpsel_c(query, 
	    ERR_MSG_LEN, 
	    SPICE_EK_TSTRLN, 
	    SPICE_EK_CSTRLN, 
	    &tN,
	    tXbegs,
	    tXends,
	    tXtypes,
	    tXclass,
	    tTabs, tCols, &tError, tErrMsg);

   ads_checkForError();

   errmsg = CORBA::string_dup(tErrMsg);

   error = static_cast<CORBA::Boolean>(tError);
   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);

   // Convert the arrays to Seqs

   CORBA::ULong n_ul = static_cast<CORBA::ULong>( tN );

   tabs   = new jpl::mipl::spice::corba::stringSeq();
   cols   = new jpl::mipl::spice::corba::stringSeq();
   xbegs  = new jpl::mipl::spice::corba::SpIntSeq();
   xends  = new jpl::mipl::spice::corba::SpIntSeq();
   xtypes = new jpl::mipl::spice::corba::EKDataTypeSeq();
   xclass = new jpl::mipl::spice::corba::EKExprClassSeq();

   // TODO check for failed memory allocation.

   tabs->length(n_ul);
   cols->length(n_ul);
   xbegs->length(n_ul);
   xends->length(n_ul);
   xtypes->length(n_ul);
   xclass->length(n_ul);

   for (int i=0; i < n; ++i)
   {
      (*tabs)[i] = static_cast<const char*>(tTabs + (i * SPICE_EK_TSTRLN));
      (*cols)[i] = static_cast<const char*>(tCols + (i * SPICE_EK_CSTRLN));

      (*xbegs)[i] = tXbegs[i];
      (*xends)[i] = tXends[i];
      (*xtypes)[i] = Spice2CorbaEKType(tXtypes[i]);
      (*xclass)[i] = Spice2CorbaEKClass(tXclass[i]);
   }
}


        void 
ads_SpiceLib_i::el2cgv (
            const jpl::mipl::spice::corba::SpEllipse & ellipse,
            jpl::mipl::spice::corba::Vector3_out center,
            jpl::mipl::spice::corba::Vector3_out smajor,
            jpl::mipl::spice::corba::Vector3_out sminor,
            CORBA::Environment& 
          )
          throw ()
{
   SpiceEllipse tEllipse;
   memset(&tEllipse, 0, sizeof(tEllipse));
   Corba2SpiceEllipse(ellipse, tEllipse);

   el2cgv_c(&tEllipse, center, smajor, sminor);
}

        void 
ads_SpiceLib_i::et2utc (
            jpl::mipl::spice::corba::SpDouble et,
            const char * format,
            jpl::mipl::spice::corba::SpInt prec,
            CORBA::String_out utcstr,
            CORBA::Environment& 
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const size_t MAX_EXPECTED_UTCSTR_LENGTH = 80;
   SpiceChar buf[MAX_EXPECTED_UTCSTR_LENGTH];
   memset(buf, '\0', sizeof(buf));

   et2utc_c(et, format, prec, MAX_EXPECTED_UTCSTR_LENGTH, buf);
   utcstr = CORBA::string_dup(buf);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::etcal (
            jpl::mipl::spice::corba::SpDouble et,
            CORBA::String_out str,
            CORBA::Environment& 
          )
          throw ()
{
   const size_t MAX_EXPECTED_STR_LENGTH = 80;
   char tStr[MAX_EXPECTED_STR_LENGTH];
   memset(tStr, '\0', sizeof(tStr));

   etcal_c(et, sizeof(tStr), tStr);

   str = CORBA::string_dup(tStr);
}

        void 
ads_SpiceLib_i::eul2m (
            jpl::mipl::spice::corba::SpDouble angle3,
            jpl::mipl::spice::corba::SpDouble angle2,
            jpl::mipl::spice::corba::SpDouble angle1,
            jpl::mipl::spice::corba::SpInt axis3,
            jpl::mipl::spice::corba::SpInt axis2,
            jpl::mipl::spice::corba::SpInt axis1,
            jpl::mipl::spice::corba::Matrix33_out r,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   eul2m_c(angle3, angle2, angle1, axis3, axis2, axis1, r);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::eul2xf (
            const jpl::mipl::spice::corba::SpDouble6 eulang,
            jpl::mipl::spice::corba::SpInt axisa,
            jpl::mipl::spice::corba::SpInt axisb,
            jpl::mipl::spice::corba::SpInt axisc,
            jpl::mipl::spice::corba::Matrix66_out xform,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   eul2xf_c(eulang, axisa, axisb, axisc, xform);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::expool (
            const char * name,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   expool_c(name, &tFound);
   found = static_cast<CORBA::Boolean>(tFound);

   ads_checkForError();
}



        void 
ads_SpiceLib_i::frame (
            jpl::mipl::spice::corba::Vector3 x,
            jpl::mipl::spice::corba::Vector3_out y,
            jpl::mipl::spice::corba::Vector3_out z,
            CORBA::Environment& 
          )
          throw ()
{
   frame_c(x, y, z);
}

        void 
ads_SpiceLib_i::frinfo (
            jpl::mipl::spice::corba::SpInt frcode,
            jpl::mipl::spice::corba::SpInt_out cent,
            jpl::mipl::spice::corba::SpInt_out frclss,
            jpl::mipl::spice::corba::SpInt_out clssid,
            CORBA::Boolean_out found,
            CORBA::Environment& 
          )
          throw ()
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tCent = 0;
   SpiceInt tFrclss = 0;
   SpiceInt tClssid = 0;
   

   frinfo_c(frcode, &tCent, &tFrclss, &tClssid, &tFound);

   found = static_cast<CORBA::Boolean>(tFound);
   cent = static_cast<jpl::mipl::spice::corba::SpInt>(tCent);
   frclss = static_cast<jpl::mipl::spice::corba::SpInt>(tFrclss);
   clssid = static_cast<jpl::mipl::spice::corba::SpInt>(tClssid);
}

void 
ads_SpiceLib_i::frmnam (
	       jpl::mipl::spice::corba::SpInt frcode,
	       CORBA::String_out frname,
	       CORBA::Environment&
	       )
  throw ()
{
  const size_t FRNAME_STRING_LENGTH = 128;
  SpiceChar tFrname[FRNAME_STRING_LENGTH];

  frmnam_c( frcode, sizeof(tFrname), tFrname);
  ads_checkForError();

  frname = CORBA::string_dup(tFrname);
}


        void 
ads_SpiceLib_i::gcpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt lenout,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::stringSeq_out cvals,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tN = 0;

   size_t lenCvals = room * lenout;
   SpiceChar* tCvals = new char[lenCvals];
   memset(tCvals, '\0', lenCvals);

   gcpool_c(name, start, room, lenout, &tN, tCvals, &tFound);
   found = static_cast<CORBA::Boolean>(tFound);


   // Convert the char arrays to StrSeqs

   cvals = new jpl::mipl::spice::corba::stringSeq;
   cvals->length(static_cast<CORBA::ULong>(tN));
   for (int i=0; i < static_cast<int>(tN); ++i)
   {
      (*cvals)[i] = static_cast<const char*>(tCvals + (i * lenout));
   }
   delete[] tCvals;

   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);
   ads_checkForError();
}


        void 
ads_SpiceLib_i::gdpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpDoubleSeq_out values,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tN = 0;
   
   gdpool_c(name, start, room, &tN, values->get_buffer(), &tFound);
   
   found = static_cast<CORBA::Boolean>(tFound);
   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::georec (
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble lat,
            jpl::mipl::spice::corba::SpDouble alt,
            jpl::mipl::spice::corba::SpDouble re,
            jpl::mipl::spice::corba::SpDouble f,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   georec_c(lon, lat, alt, re, f, rectan);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::getfov (
            jpl::mipl::spice::corba::SpInt instid,
            jpl::mipl::spice::corba::SpInt room,
            CORBA::String_out shape,
            CORBA::String_out frame,
            jpl::mipl::spice::corba::Vector3_out bsight,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::Vector3Seq_out bounds,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const int EXPECTED_STRING_LENGTH = 64;
   SpiceChar tShape[EXPECTED_STRING_LENGTH];
   memset(tShape, '\0', sizeof(tShape));
   SpiceChar tFrame[EXPECTED_STRING_LENGTH];
   memset(tFrame, '\0', sizeof(tFrame));

   SpiceInt tN = 0;
   
   getfov_c(instid, room, sizeof(shape), sizeof(frame), tShape, tFrame,
	    bsight, &tN, bounds->get_buffer());

   shape = CORBA::string_dup(tShape);
   frame = CORBA::string_dup(tFrame);
   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);
   ads_checkForError();
}


        void 
ads_SpiceLib_i::gipool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpIntSeq_out ivals,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceInt tN = 0;
   SpiceBoolean tFound = SPICEFALSE;

   SpiceInt* tIvals = new SpiceInt[room];
   if (tIvals == 0)
   {
      // TODO bad alloc?
      return;
   }

   gipool_c(name, start, room, &tN, tIvals, &tFound);

   memcpy(ivals->get_buffer(), tIvals, room * sizeof(SpiceInt));
   delete [] tIvals;

   found = static_cast<CORBA::Boolean>(tFound);
   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);

   ads_checkForError();
}



        void 
ads_SpiceLib_i::gnpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt lenout,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::stringSeq_out kvars,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tN = 0;

   size_t lenKvars = room * lenout;
   SpiceChar* tKvars = new char[lenKvars];
   memset(tKvars, '\0', lenKvars);

   gnpool_c(name, start, room, lenout, &tN, tKvars, &tFound);

   found = static_cast<CORBA::Boolean>(tFound);
   n = static_cast<jpl::mipl::spice::corba::SpInt>(tN);

   // Convert the char arrays to StrSeqs

   kvars = new jpl::mipl::spice::corba::stringSeq;
   kvars->length(static_cast<int>(tN));
   
   for (int i=0; i < static_cast<int>(tN); ++i)
   {
      (*kvars)[i] = static_cast<const char*>(tKvars + (i * lenout));
   }
   delete[] tKvars;

   ads_checkForError();
}


        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::halfpi (
            CORBA::Environment& 
          )
          throw ()
{
   return halfpi_c();
}

        void 
ads_SpiceLib_i::ident (
            jpl::mipl::spice::corba::Matrix33_out matrix,
            CORBA::Environment& 
          )
          throw ()
{
   ident_c(matrix);
}

        void 
ads_SpiceLib_i::illum (
            const char * target,
            jpl::mipl::spice::corba::SpDouble et,
            const char * abcorr,
            const char * obsrvr,
            const jpl::mipl::spice::corba::Vector3 spoint,
            jpl::mipl::spice::corba::SpDouble_out phase,
            jpl::mipl::spice::corba::SpDouble_out solar,
            jpl::mipl::spice::corba::SpDouble_out emissn,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   illum_c(target, et, abcorr, obsrvr, spoint, &phase, &solar, &emissn);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::inedpl (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::SpEllipse_out ellipse,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceEllipse tEllipse;
   memset(&tEllipse, 0, sizeof(tEllipse));
   
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   inedpl_c(a, b, c, &tPlane, &tEllipse, &tFound);
   ads_checkForError();

   Spice2CorbaEllipse(tEllipse, ellipse);
   found = static_cast<CORBA::Boolean>(tFound);


}

void 
ads_SpiceLib_i::inelpl (
	       const jpl::mipl::spice::corba::SpEllipse & ellips,
	       const jpl::mipl::spice::corba::SpPlane & plane,
	       jpl::mipl::spice::corba::SpInt_out nxpts,
	       jpl::mipl::spice::corba::Vector3_out xpt1,
	       jpl::mipl::spice::corba::Vector3_out xpt2,
	       CORBA::Environment &
            )
  throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) 
{
   SpiceEllipse tEllips;
   memset(&tEllips, 0, sizeof(tEllips));
   Corba2SpiceEllipse(ellips, tEllips);

   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   SpiceInt tNxpts = 0;

   inelpl_c(&tEllips, &tPlane, &tNxpts, xpt1, xpt2);
   ads_checkForError();

   nxpts = tNxpts;
}

        void 
ads_SpiceLib_i::inrypl (
            const jpl::mipl::spice::corba::Vector3 vertex,
            const jpl::mipl::spice::corba::Vector3 dir,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::SpInt_out nxpts,
            jpl::mipl::spice::corba::Vector3_out xpt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   SpiceInt tNxpts = 0;

   inrypl_c(vertex, dir, &tPlane, &tNxpts, xpt);

   nxpts = static_cast<jpl::mipl::spice::corba::SpInt>(tNxpts);
   ads_checkForError();
}

        jpl::mipl::spice::corba::SpInt 
ads_SpiceLib_i::intmax (
            CORBA::Environment& 
          )
          throw ()
{
   return intmax_c();
}

        jpl::mipl::spice::corba::SpInt 
ads_SpiceLib_i::intmin (
            CORBA::Environment& 
          )
          throw ()
{
   return intmin_c();
}

        void 
ads_SpiceLib_i::invert (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   invert_c(m1, mout);
}

        CORBA::Boolean 
ads_SpiceLib_i::isrot (
            const jpl::mipl::spice::corba::Matrix33 m,
            jpl::mipl::spice::corba::SpDouble ntol,
            jpl::mipl::spice::corba::SpDouble dtol,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean ret = isrot_c(m, ntol, dtol);
  
   ads_checkForError();
   return static_cast<CORBA::Boolean>(ret);

}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::j1900 (
            CORBA::Environment& 
          )
          throw ()
{
   return j1900_c();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::j1950 (
            CORBA::Environment& 
          )
          throw ()
{
   return j1950_c();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::j2000 (
            CORBA::Environment& 
          )
          throw ()
{
   return j2000_c();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::j2100 (
            CORBA::Environment& 
          )
          throw ()
{
   return j2100_c();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::jyear (
            CORBA::Environment& 
          )
          throw ()
{
   return jyear_c();
}

        void 
ads_SpiceLib_i::kdata (
            jpl::mipl::spice::corba::SpInt which,
            const char * kind,
            CORBA::String_out file,
            CORBA::String_out filtyp,
            CORBA::String_out source,
            jpl::mipl::spice::corba::SpInt_out handle,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tHandle = 0;
   
   const size_t MAX_EXPECTED_STR_LENGTH = 80;
   char tFile[MAX_EXPECTED_STR_LENGTH];
   memset(tFile, '\0', sizeof(tFile));
   char tFiltyp[MAX_EXPECTED_STR_LENGTH];
   memset(tFiltyp, '\0', sizeof(tFiltyp));
   char tSource[MAX_EXPECTED_STR_LENGTH];
   memset(tSource, '\0', sizeof(tSource));

   kdata_c(which, 
	   kind, 
	   MAX_EXPECTED_STR_LENGTH,
	   MAX_EXPECTED_STR_LENGTH,
	   MAX_EXPECTED_STR_LENGTH,
	   tFile, 
	   tFiltyp, 
	   tSource,
	   &tHandle, 
	   &tFound);

   file = CORBA::string_dup(tFile);
   filtyp = CORBA::string_dup(tFiltyp);
   source = CORBA::string_dup(tSource);
   
   found = static_cast<CORBA::Boolean>(tFound);
   handle = static_cast<jpl::mipl::spice::corba::SpInt>(tHandle);
   
   ads_checkForError();
}

        void 
ads_SpiceLib_i::ktotal (
            const char * kind,
            jpl::mipl::spice::corba::SpInt_out count,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceInt tCount = 0;

   ktotal_c(kind, &tCount);

   count = static_cast<jpl::mipl::spice::corba::SpInt>(tCount);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::latcyl (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble lat,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out lonc,
            jpl::mipl::spice::corba::SpDouble_out z,
            CORBA::Environment& 
          )
          throw ()
{
   latcyl_c(radius, lon, lat, &r, &lonc, &z);
}

        void 
ads_SpiceLib_i::latrec (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble longitude,
            jpl::mipl::spice::corba::SpDouble latitude,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment& 
          )
          throw ()
{
   latrec_c(radius, longitude, latitude, rectan);
}

        void 
ads_SpiceLib_i::latsph (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble lat,
            jpl::mipl::spice::corba::SpDouble_out rho,
            jpl::mipl::spice::corba::SpDouble_out colat,
            jpl::mipl::spice::corba::SpDouble_out lons,
            CORBA::Environment& 
          )
          throw ()
{
   latsph_c(radius, lon, lat, &rho, &colat, &lons);
}

        void 
ads_SpiceLib_i::ltime (
            jpl::mipl::spice::corba::SpDouble etobs,
            jpl::mipl::spice::corba::SpInt obs,
            const char * dir,
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble_out ettarg,
            jpl::mipl::spice::corba::SpDouble_out elapsd,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   ltime_c(etobs, obs, dir, targ, &ettarg, &elapsd);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::m2eul (
            const jpl::mipl::spice::corba::Matrix33 r,
            jpl::mipl::spice::corba::SpInt axis3,
            jpl::mipl::spice::corba::SpInt axis2,
            jpl::mipl::spice::corba::SpInt axis1,
            jpl::mipl::spice::corba::SpDouble_out angle3,
            jpl::mipl::spice::corba::SpDouble_out angle2,
            jpl::mipl::spice::corba::SpDouble_out angle1,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   m2eul_c(r, axis3, axis2, axis1, &angle3, &angle2, &angle1);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::m2q (
            const jpl::mipl::spice::corba::Matrix33 r,
            jpl::mipl::spice::corba::Quaternion_out q,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   m2q_c(r, q);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::mequ (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   mequ_c(m1, mout);
}

        void 
ads_SpiceLib_i::mtxm (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Matrix33 m2,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   mtxm_c(m1, m2, mout);
}

        void 
ads_SpiceLib_i::mtxv (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Vector3 vin,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   mtxv_c(m1, vin, vout);
}

        void 
ads_SpiceLib_i::mxm (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Matrix33 m2,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   mxm_c(m1, m2, mout);
}

        void 
ads_SpiceLib_i::mxmt (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Matrix33 m2,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   mxmt_c(m1, m2, mout);
}

        void 
ads_SpiceLib_i::mxv (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Vector3 vin,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   mxv_c(m1, vin, vout);
}


void 
ads_SpiceLib_i::namfrm (
	   const char * frname,
	   jpl::mipl::spice::corba::SpInt_out frcode,
	   CORBA::Environment &
	   )
  throw ()
{
  SpiceInt tFrcode = 0;

  namfrm_c (frname, &tFrcode);
  
  frcode = tFrcode;
}

        void 
ads_SpiceLib_i::nearpt (
            const jpl::mipl::spice::corba::Vector3 positn,
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            jpl::mipl::spice::corba::Vector3_out npoint,
            jpl::mipl::spice::corba::SpDouble_out alt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   nearpt_c(positn, a, b, c, npoint, &alt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::npedln (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 linept,
            const jpl::mipl::spice::corba::Vector3 linedr,
            jpl::mipl::spice::corba::Vector3_out pnear,
            jpl::mipl::spice::corba::SpDouble_out dist,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   npedln_c(a, b, c, linept, linedr, pnear, &dist);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::npelpt (
            const jpl::mipl::spice::corba::Vector3 point,
            const jpl::mipl::spice::corba::SpEllipse & ellips,
            jpl::mipl::spice::corba::Vector3_out pnear,
            jpl::mipl::spice::corba::SpDouble_out dist,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceEllipse tEllips;
   memset(&tEllips, 0, sizeof(tEllips));
   Corba2SpiceEllipse(ellips, tEllips);

   npelpt_c(point, &tEllips, pnear, &dist);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::nplnpt (
            const jpl::mipl::spice::corba::Vector3 linpt,
            const jpl::mipl::spice::corba::Vector3 lindir,
            const jpl::mipl::spice::corba::Vector3 point,
            jpl::mipl::spice::corba::Vector3_out pnear,
            jpl::mipl::spice::corba::Vector3_out dist,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   nplnpt_c(linpt, lindir, point, pnear, dist);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::nvc2pl (
            const jpl::mipl::spice::corba::Vector3 normal,
            jpl::mipl::spice::corba::SpDouble constant,
            jpl::mipl::spice::corba::SpPlane_out plane,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   
   nvc2pl_c(normal, constant, &tPlane);
   ads_checkForError();

   Spice2CorbaPlane(tPlane, plane);
}

        void 
ads_SpiceLib_i::nvp2pl (
            const jpl::mipl::spice::corba::Vector3 normal,
            const jpl::mipl::spice::corba::Vector3 point,
            jpl::mipl::spice::corba::SpPlane_out plane,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));

   nvp2pl_c(normal, point, &tPlane);
   ads_checkForError();

   Spice2CorbaPlane(tPlane, plane);
}

        void 
ads_SpiceLib_i::oscelt (
            const jpl::mipl::spice::corba::State theState,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::SpDouble mu,
            jpl::mipl::spice::corba::SpDouble8_out elts,
            CORBA::Environment &
          )
          throw ()
{
   oscelt_c(theState, et, mu, elts);
   ads_checkForError();
}


        void 
ads_SpiceLib_i::pcpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt n,
            jpl::mipl::spice::corba::SpInt lenvals,
            const jpl::mipl::spice::corba::stringSeq & cvals,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   size_t lenCvals = n * lenvals;
   SpiceChar* tCvals = new char[lenCvals];
   memset(tCvals, '\0', lenCvals);

   // Copy the stringSeq to char array.

   for (int i=0; i < static_cast<int>(cvals.length()); ++i)
   {
      strncpy(tCvals + (i * lenvals), cvals[i], lenvals);
   }

   pcpool_c(name, n, lenvals, tCvals);

   delete[] tCvals;

   ads_checkForError();


}


        void 
ads_SpiceLib_i::pdpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt n,
            const jpl::mipl::spice::corba::SpDoubleSeq & dvals,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{

   // TODO tell NAIF that arg 3 should be ConstSpiceDouble*, not SpiceDouble*

   SpiceDouble* tDvals = new SpiceDouble[dvals.length()];
   memset(tDvals, 0,                  dvals.length() * sizeof(SpiceDouble));
   memcpy(tDvals, dvals.get_buffer(), dvals.length() * sizeof(SpiceDouble));

   pdpool_c(name, n, tDvals);

   delete tDvals;

   ads_checkForError();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::pi (
            CORBA::Environment& 
          )
          throw ()
{
   return pi_c();
}

        void 
ads_SpiceLib_i::pipool (
            const char * name,
            jpl::mipl::spice::corba::SpInt n,
            const jpl::mipl::spice::corba::SpIntSeq & ivals,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceInt* tIvals = new SpiceInt[ivals.length()];
   memset(tIvals, 0,                  ivals.length() * sizeof(SpiceInt));
   memcpy(tIvals, ivals.get_buffer(), ivals.length() * sizeof(SpiceInt));

   pipool_c(name, n, tIvals);

   delete tIvals;

   ads_checkForError();
}

        void 
ads_SpiceLib_i::pjelpl (
            const jpl::mipl::spice::corba::SpEllipse & elin,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::SpEllipse_out elout,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceEllipse tElout;
   memset(&tElout, 0, sizeof(tElout));

   SpiceEllipse tElin;
   memset(&tElin, 0, sizeof(tElin));
   Corba2SpiceEllipse(elin, tElin);
   
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   pjelpl_c(&tElin, &tPlane, &tElout);
   ads_checkForError();

   Spice2CorbaEllipse(tElout, elout);
}

        void 
ads_SpiceLib_i::pl2nvc (
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out normal,
            jpl::mipl::spice::corba::SpDouble_out constant,
            CORBA::Environment& 
          )
          throw ()
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   pl2nvc_c(&tPlane, normal, &constant);
}

        void 
ads_SpiceLib_i::pl2nvp (
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out normal,
            jpl::mipl::spice::corba::Vector3_out point,
            CORBA::Environment& 
          )
          throw ()
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   pl2nvp_c(&tPlane, normal, point);
}

        void 
ads_SpiceLib_i::pl2psv (
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out point,
            jpl::mipl::spice::corba::Vector3_out span1,
            jpl::mipl::spice::corba::Vector3_out span2,
            CORBA::Environment& 
          )
          throw ()
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   pl2psv_c(&tPlane, point, span1, span2);
}

        void 
ads_SpiceLib_i::prop2b (
            jpl::mipl::spice::corba::SpDouble gm,
            const jpl::mipl::spice::corba::State pvinit,
            jpl::mipl::spice::corba::SpDouble dt,
            jpl::mipl::spice::corba::State_out pvprop,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   jpl::mipl::spice::corba::State tPvinit;
   memset(tPvinit, 0, sizeof(tPvinit));
   memcpy(tPvinit, pvinit, sizeof(tPvinit));

   // TODO tell NAIF that param 2 should be const double* not double*.

   prop2b_c(gm, tPvinit, dt, pvprop);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::psv2pl (
            const jpl::mipl::spice::corba::Vector3 point,
            const jpl::mipl::spice::corba::Vector3 span1,
            const jpl::mipl::spice::corba::Vector3 span2,
            jpl::mipl::spice::corba::SpPlane_out plane,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));

   psv2pl_c(point, span1, span2, &tPlane);
   ads_checkForError();

   Spice2CorbaPlane(tPlane, plane);
}

        void 
ads_SpiceLib_i::pxform (
            const char * from,
            const char * to,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix33_out rotate,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   pxform_c(from, to, et, rotate);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::q2m (
            const jpl::mipl::spice::corba::Quaternion q,
            jpl::mipl::spice::corba::Matrix33_out r,
            CORBA::Environment& 
          )
          throw ()
{
   // TODO tell NAIF that param 1 should be ConstSpiceDouble not SpiceDouble

   SpiceDouble tQ[4];
   memset(tQ, 0, sizeof(tQ));
   memcpy(tQ, q, sizeof(tQ));

   q2m_c(tQ, r);
}

        void 
ads_SpiceLib_i::radrec (
            jpl::mipl::spice::corba::SpDouble range,
            jpl::mipl::spice::corba::SpDouble ra,
            jpl::mipl::spice::corba::SpDouble dec,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment& 
          )
          throw ()
{
   radrec_c(range, ra, dec, rectan);
}

        void 
ads_SpiceLib_i::rav2xf (
            const jpl::mipl::spice::corba::Matrix33 rot,
            const jpl::mipl::spice::corba::Vector3 av,
            jpl::mipl::spice::corba::Matrix66_out xform,
            CORBA::Environment& 
          )
          throw ()
{
   rav2xf(rot, av, xform);
}

        void 
ads_SpiceLib_i::raxisa (
            const jpl::mipl::spice::corba::Matrix33 matrix,
            jpl::mipl::spice::corba::Vector3_out axis,
            jpl::mipl::spice::corba::SpDouble_out angle,
            CORBA::Environment& 
          )
          throw ()
{
   raxisa_c(matrix, axis, &angle);
}

        void 
ads_SpiceLib_i::reccyl (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out z,
            CORBA::Environment& 
          )
          throw ()
{
   reccyl_c(rectan, &r, &lon, &z);
}

        void 
ads_SpiceLib_i::recgeo (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble re,
            jpl::mipl::spice::corba::SpDouble f,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out lat,
            jpl::mipl::spice::corba::SpDouble_out alt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   recgeo_c(rectan, re, f, &lon, &lat, &alt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::reclat (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out longitude,
            jpl::mipl::spice::corba::SpDouble_out latitude,
            CORBA::Environment& 
          )
          throw ()
{
   reclat_c(rectan, &radius, &longitude, &latitude);
}

        void 
ads_SpiceLib_i::recrad (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out range,
            jpl::mipl::spice::corba::SpDouble_out ra,
            jpl::mipl::spice::corba::SpDouble_out dec,
            CORBA::Environment& 
          )
          throw ()
{
   recrad_c(rectan, &range, &ra, &dec);
}

        void 
ads_SpiceLib_i::recsph (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out colat,
            jpl::mipl::spice::corba::SpDouble_out lon,
            CORBA::Environment& 
          )
          throw ()
{
   recsph_c(rectan, &r, &colat, &lon);
}

        void 
ads_SpiceLib_i::rotate (
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::SpInt iaxis,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   rotate_c(angle, iaxis, mout);
}

       void 
ads_SpiceLib_i::rotmat (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::SpInt iaxis,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   rotmat_c(m1, angle, iaxis, mout);
}


        void 
ads_SpiceLib_i::rotvec (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::SpInt iaxis,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   rotvec_c(v1, angle, iaxis, vout);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::rpd (
            CORBA::Environment& 
          )
          throw ()
{
   return rpd_c();
}

        void 
ads_SpiceLib_i::rquad (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            jpl::mipl::spice::corba::SpDouble2_out root1,
            jpl::mipl::spice::corba::SpDouble2_out root2,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   rquad_c(a, b, c, root1, root2);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::saelgv (
            const jpl::mipl::spice::corba::Vector3 vec1,
            const jpl::mipl::spice::corba::Vector3 vec2,
            jpl::mipl::spice::corba::Vector3_out smajor,
            jpl::mipl::spice::corba::Vector3_out sminor,
            CORBA::Environment& 
          )
          throw ()
{
   saelgv_c(vec1, vec2, smajor, sminor);
}

        void 
ads_SpiceLib_i::scdecd (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            CORBA::String_out sclkch,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const SpiceInt TIME_STRING_LENGTH = 128;
   SpiceChar tSclkch[TIME_STRING_LENGTH];
   memset(tSclkch, '\0', sizeof(tSclkch));

   scdecd_c(sc, sclkdp, sizeof(tSclkch), tSclkch);
   ads_checkForError();

   sclkch = CORBA::string_dup(tSclkch);
}

        void 
ads_SpiceLib_i::sce2c (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::SpDouble_out sclkdp,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   sce2c_c(sc, et, &sclkdp);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::sce2s (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble et,
            CORBA::String_out sclkch,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const size_t SCLKCH_STRING_LENGTH = 128;
   SpiceChar tSclkch[SCLKCH_STRING_LENGTH];
   memset(tSclkch, '\0', sizeof(tSclkch));

   sce2s_c(sc, et, sizeof(tSclkch), tSclkch);
   ads_checkForError();

   sclkch = CORBA::string_dup(tSclkch);
}

        void 
ads_SpiceLib_i::scencd (
            jpl::mipl::spice::corba::SpInt sc,
            const char * sclkch,
            jpl::mipl::spice::corba::SpDouble_out sclkdp,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   scencd_c(sc, sclkch, &sclkdp);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::scfmt (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble ticks,
            CORBA::String_out clkstr,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const size_t CLKSTR_STRING_LENGTH = 128;
   SpiceChar tClkstr[CLKSTR_STRING_LENGTH];
   memset(tClkstr, '\0', sizeof(tClkstr));

   scfmt_c(sc, ticks, sizeof(tClkstr), tClkstr);
   ads_checkForError();

   clkstr = CORBA::string_dup(tClkstr);
}

        void 
ads_SpiceLib_i::scpart (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpInt_out nparts,
            jpl::mipl::spice::corba::SpDouble_out pstart,
            jpl::mipl::spice::corba::SpDouble_out pstop,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceInt tNparts = 0;
   
   scpart_c(sc, &tNparts, &pstart, &pstop);
   
   nparts = static_cast<jpl::mipl::spice::corba::SpInt>(tNparts);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::scs2e (
            jpl::mipl::spice::corba::SpInt sc,
            const char * sclkch,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   scs2e_c(sc, sclkch, &et);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::sct2e (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   sct2e_c(sc, sclkdp, &et);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::sctiks (
            jpl::mipl::spice::corba::SpInt sc,
            const char * clkstr,
            jpl::mipl::spice::corba::SpDouble_out ticks,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   sctiks_c(sc, clkstr, &ticks);
   ads_checkForError();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::spd (
            CORBA::Environment& 
          )
          throw ()
{
   return spd_c();
}

        void 
ads_SpiceLib_i::sphcyl (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble colat,
            jpl::mipl::spice::corba::SpDouble slon,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out z,
            CORBA::Environment& 
          )
          throw ()
{
   sphcyl_c(radius, colat, slon, &r, &lon, &z);
}

        void 
ads_SpiceLib_i::sphlat (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble colat,
            jpl::mipl::spice::corba::SpDouble lons,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out lat,
            CORBA::Environment& 
          )
          throw ()
{
   sphlat_c(r, colat, lons, &radius, &lon, &lat); 
}

        void 
ads_SpiceLib_i::sphrec (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble colat,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment& 
          )
          throw ()
{
   sphrec_c(r, colat, lon, rectan);
}

        void 
ads_SpiceLib_i::spkapo (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const jpl::mipl::spice::corba::State sobs,
            const char * abcorr,
            jpl::mipl::spice::corba::Vector3_out ptarg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkapo_c(targ, et, ref, sobs, abcorr, ptarg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkapp (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const jpl::mipl::spice::corba::State sobs,
            const char * abcorr,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkapp_c(targ, et, ref, sobs, abcorr, starg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkez (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkez_c(targ, et, ref, abcorr, obs, starg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkezp (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::Vector3_out ptarg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkezp_c(targ, et, ref, abcorr, obs, ptarg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkezr (
            const char * targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            const char * obs,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkezr_c(targ, et, ref, abcorr, obs, starg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkgeo (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkgeo_c(targ, et, ref, obs, starg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkgps (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::Vector3_out pos,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkgps_c(targ, et, ref, obs, pos, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkpos (
            const char * targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            const char * obs,
            jpl::mipl::spice::corba::Vector3_out ptarg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkpos_c(targ, et, ref, abcorr, obs, ptarg, &lt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::spkssb (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            jpl::mipl::spice::corba::State_out starg,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   spkssb_c(targ, et, ref, starg);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::stelab (
            const jpl::mipl::spice::corba::Vector3 pobj,
            const jpl::mipl::spice::corba::Vector3 vobs,
            jpl::mipl::spice::corba::Vector3_out appobj,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   stelab(pobj, vobs, appobj);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::stpool (
            const char * item,
            jpl::mipl::spice::corba::SpInt nth,
            const char * contin,
            jpl::mipl::spice::corba::SpInt /*lenout*/,
            CORBA::String_out str,
            jpl::mipl::spice::corba::SpInt_out size,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const size_t STR_LENGTH = 256;
   SpiceChar tStr[STR_LENGTH];
   memset(tStr, '\0', sizeof(tStr));

   SpiceBoolean tFound = SPICEFALSE;
   SpiceInt tSize = 0;

   stpool_c(item, nth, contin, sizeof(tStr), tStr, &tSize, &tFound);
   ads_checkForError();

   found = static_cast<CORBA::Boolean>(tFound);
   size = static_cast<jpl::mipl::spice::corba::SpInt>(tSize);
   str = CORBA::string_dup(tStr);
   
}

        void 
ads_SpiceLib_i::str2et (
            const char * str,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   str2et_c(str, &et);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::subpt (
            const char * method,
            const char * target,
            jpl::mipl::spice::corba::SpDouble et,
            const char * abcorr,
            const char * obsrvr,
            jpl::mipl::spice::corba::Vector3_out spoint,
            jpl::mipl::spice::corba::SpDouble_out alt,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   subpt(method, target, et, abcorr, obsrvr, spoint, alt);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::subsol (
            const char * method,
            const char * target,
            jpl::mipl::spice::corba::SpDouble et,
            const char * abcorr,
            const char * obsrvr,
            jpl::mipl::spice::corba::Vector3_out spoint,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   subsol_c(method, target, et, abcorr, obsrvr, spoint);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::surfnm (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 point,
            jpl::mipl::spice::corba::Vector3_out normal,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   surfnm_c(a, b, c, point, normal);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::surfpt (
            const jpl::mipl::spice::corba::Vector3 positn,
            const jpl::mipl::spice::corba::Vector3 u,
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            jpl::mipl::spice::corba::Vector3_out point,
            CORBA::Boolean_out found,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   SpiceBoolean tFound = SPICEFALSE;
   surfpt_c(positn, u, a, b, c, point, &tFound);
   found = static_cast<CORBA::Boolean>(tFound);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::sxform (
            const char * from,
            const char * to,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix66_out xform,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   sxform(from, to, et, xform);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::timdef (
            const char * action,
            const char * item,
            jpl::mipl::spice::corba::SpInt lenout,
            CORBA::String_out value,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   // TODO allocate space for String_out

   timdef(action, item, lenout, value);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::timout (
            jpl::mipl::spice::corba::SpDouble et,
            const char * pictur,
            CORBA::String_out output,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   const int MAX_LENGTH = 256;
   SpiceChar buf[MAX_LENGTH];
   memset(buf, '\0', sizeof(buf));

   timout_c(et, pictur, MAX_LENGTH, buf);
   ads_checkForError();

   output = CORBA::string_dup(buf);
}

        void 
ads_SpiceLib_i::tipbod (
            const char * ref,
            jpl::mipl::spice::corba::SpInt body,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix33_out tipm,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   tipbod_c(ref, body, et, tipm);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::tisbod (
            const char * ref,
            jpl::mipl::spice::corba::SpInt body,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix66_out tsipm,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   tisbod_c(ref, body, et, tsipm);
   ads_checkForError();
}

        char * 
ads_SpiceLib_i::tkvrsn (
            const char * item,
            CORBA::Environment& 
          )
          throw ()
{
   return CORBA::string_dup( tkvrsn_c(item) );
}

        void 
ads_SpiceLib_i::tparse (
            const char * str,
             jpl::mipl::spice::corba::SpDouble_out sp2000,
            CORBA::String_out errmsg,
            CORBA::Environment& 
          )
          throw ()
{
   const size_t ERRMSG_LENGTH = 128;
   SpiceChar tErrmsg[ERRMSG_LENGTH];
   memset(tErrmsg, '\0', sizeof(tErrmsg));

   tparse_c(str, sizeof(tErrmsg), &sp2000, tErrmsg);

   errmsg = CORBA::string_dup(tErrmsg);
}

        void 
ads_SpiceLib_i::tpictr (
            const char * sample,
            CORBA::String_out pictur,
            CORBA::Boolean_out ok,
            CORBA::String_out errmsg,
            CORBA::Environment& 
          )
          throw ()
{
   const size_t PICTUR_LENGTH = 128;
   SpiceChar tPictur[PICTUR_LENGTH];
   memset(tPictur, '\0', sizeof(tPictur));
   const size_t ERRMSG_LENGTH = 128;
   SpiceChar tErrmsg[ERRMSG_LENGTH];
   memset(tErrmsg, '\0', sizeof(tErrmsg));


   SpiceBoolean tOk = SPICEFALSE;
   tpictr_c(sample, sizeof(tPictur), sizeof(tErrmsg), tPictur, &tOk, tErrmsg);
   ok = static_cast<CORBA::Boolean>(tOk);

   pictur = CORBA::string_dup(tPictur);
   errmsg = CORBA::string_dup(tErrmsg);
}

        void 
ads_SpiceLib_i::tsetyr (
            jpl::mipl::spice::corba::SpInt year,
            CORBA::Environment& 
          )
          throw ()
{
   tsetyr(year);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::twopi (
            CORBA::Environment& 
          )
          throw ()
{
   return twopi_c();
}

        void 
ads_SpiceLib_i::twovec (
            const jpl::mipl::spice::corba::Vector3 axdef,
            jpl::mipl::spice::corba::SpInt indexa,
            const jpl::mipl::spice::corba::Vector3 plndef,
            jpl::mipl::spice::corba::SpInt indexp,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   twovec_c(axdef, indexa, plndef, indexp, mout);
   ads_checkForError();
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::tyear (
            CORBA::Environment& 
          )
          throw ()
{
   return tyear_c();
}

        void 
ads_SpiceLib_i::ucrss (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   ucrss_c(v1, v2, vout);
}


        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::unitim (
            jpl::mipl::spice::corba::SpDouble epoch,
            const char * insys,
            const char * outsys,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   jpl::mipl::spice::corba::SpDouble ret = unitim_c(epoch, insys, outsys);
   ads_checkForError();
   return ret;
}

        void 
ads_SpiceLib_i::unorm (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            jpl::mipl::spice::corba::SpDouble_out vmag,
            CORBA::Environment& 
          )
          throw ()
{
   unorm_c(v1, vout, &vmag);
}

       void 
ads_SpiceLib_i::utc2et (
            const char * utcstr,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException )
{
   utc2et_c(utcstr, &et);
   ads_checkForError();
}

        void 
ads_SpiceLib_i::vadd (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vadd_c(v1, v2, vout);
}

        void 
ads_SpiceLib_i::vcrss (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vcrss_c(v1, v2, vout);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::vdist (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment& 
          )
          throw ()
{
   return vdist_c(v1, v2);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::vdot (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment& 
          )
          throw ()
{
   return vdot_c(v1, v2);
}

        void 
ads_SpiceLib_i::vequ (
            const jpl::mipl::spice::corba::Vector3 vin,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vequ_c(vin, vout);
}

        void 
ads_SpiceLib_i::vhat (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vhat_c(v1, vout);
}

        void 
ads_SpiceLib_i::vlcom3 (
            jpl::mipl::spice::corba::SpDouble a,
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::SpDouble b,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 v3,
            jpl::mipl::spice::corba::Vector3_out sum,
            CORBA::Environment& 
          )
          throw ()
{
   vlcom3_c(a, v1, b, v2, c, v3, sum);
}

        void 
ads_SpiceLib_i::vlcom (
            jpl::mipl::spice::corba::SpDouble a,
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::SpDouble b,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out sum,
            CORBA::Environment& 
          )
          throw ()
{
   vlcom_c(a, v1, b, v2, sum);
}

        void 
ads_SpiceLib_i::vminus (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vminus_c(v1, vout);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::vnorm (
            const jpl::mipl::spice::corba::Vector3 v1,
            CORBA::Environment& 
          )
          throw ()
{
   return vnorm_c(v1);
}

        void 
ads_SpiceLib_i::vperp (
            const jpl::mipl::spice::corba::Vector3 a,
            const jpl::mipl::spice::corba::Vector3 b,
            jpl::mipl::spice::corba::Vector3_out p,
            CORBA::Environment& 
          )
          throw ()
{
   vperp_c(a, b, p);
}

        void 
ads_SpiceLib_i::vprjp (
            const jpl::mipl::spice::corba::Vector3 vin,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   SpicePlane tPlane;
   memset(&tPlane, 0, sizeof(tPlane));
   Corba2SpicePlane(plane, tPlane);

   vprjp_c (vin, &tPlane, vout);
}

        void 
ads_SpiceLib_i::vprjpi (
            const jpl::mipl::spice::corba::Vector3 vin,
            const jpl::mipl::spice::corba::SpPlane & projpl,
            const jpl::mipl::spice::corba::SpPlane & invpl,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Boolean_out found,
            CORBA::Environment& 
          )
          throw ()
{
   SpicePlane tProjpl;
   memset(&tProjpl, 0, sizeof(tProjpl));
   Corba2SpicePlane(projpl, tProjpl);

   SpicePlane tInvpl;
   memset(&tInvpl, 0, sizeof(tInvpl));
   Corba2SpicePlane(invpl, tInvpl);

   SpiceBoolean tFound = SPICEFALSE;
   vprjpi_c(vin, &tProjpl, &tInvpl, vout, &tFound);
   found = static_cast<CORBA::Boolean>(tFound);
}

        void 
ads_SpiceLib_i::vproj (
            const jpl::mipl::spice::corba::Vector3 a,
            const jpl::mipl::spice::corba::Vector3 b,
            jpl::mipl::spice::corba::Vector3_out p,
            CORBA::Environment& 
          )
          throw ()
{
   vproj_c(a, b, p);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::vrel (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment& 
          )
          throw ()
{
   return vrel_c(v1, v2);
}

        void 
ads_SpiceLib_i::vrotv (
            const jpl::mipl::spice::corba::Vector3 v,
            const jpl::mipl::spice::corba::Vector3 axis,
            jpl::mipl::spice::corba::SpDouble theta,
            jpl::mipl::spice::corba::Vector3_out r,
            CORBA::Environment& 
          )
          throw ()
{
   vrotv_c(v, axis, theta, r);
}

         void 
ads_SpiceLib_i::vscl (
            jpl::mipl::spice::corba::SpDouble s,
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vscl_c(s, v1, vout);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::vsep (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment& 
          )
          throw ()
{
   return vsep_c(v1, v2);
}

        void 
ads_SpiceLib_i::vsub (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment& 
          )
          throw ()
{
   vsub_c(v1, v2, vout);
}

        jpl::mipl::spice::corba::SpDouble 
ads_SpiceLib_i::vtmv (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Matrix33 matrix,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment& 
          )
          throw ()
{
   return vtmv_c(v1, matrix, v2);
}

        CORBA::Boolean 
ads_SpiceLib_i::vzero (
            const jpl::mipl::spice::corba::Vector3 v,
            CORBA::Environment& 
          )
          throw ()
{
   SpiceBoolean ret = vzero_c(v);
   return static_cast<CORBA::Boolean>(ret);
}

        void 
ads_SpiceLib_i::xf2eul (
            const jpl::mipl::spice::corba::Matrix66 xform,
            jpl::mipl::spice::corba::SpInt axisa,
            jpl::mipl::spice::corba::SpInt axisb,
            jpl::mipl::spice::corba::SpInt axisc,
            jpl::mipl::spice::corba::SpDouble6_out eulang,
            CORBA::Boolean_out unique,
            CORBA::Environment &
          )
          throw ()
{
   SpiceBoolean tUnique = SPICEFALSE;
   xf2eul_c(xform, axisa, axisb, axisc, eulang, &tUnique);
   unique = static_cast<CORBA::Boolean>(tUnique);

   ads_checkForError();
}

        void 
ads_SpiceLib_i::xf2rav (
            const jpl::mipl::spice::corba::Matrix66 xform,
            jpl::mipl::spice::corba::Matrix33_out rot,
            jpl::mipl::spice::corba::Vector3_out av,
            CORBA::Environment& 
          )
          throw ()
{
   xf2rav(xform, rot, av);
}

        void 
ads_SpiceLib_i::xpose6 (
            const jpl::mipl::spice::corba::Matrix66 m,
            jpl::mipl::spice::corba::Matrix66_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   xpose6_c(m, mout);
}

        void 
ads_SpiceLib_i::xpose (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment& 
          )
          throw ()
{
   xpose_c(m1, mout);
}
