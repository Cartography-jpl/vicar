// ads_DataBase.cc
//
// Apr 25, 2002
// Michael Brady

#include "ads_DataBase.h"

#include "ads_error.h"
#include "ads_TraceAdder.h"
#include "SpiceUsr.h"

#include <string>

using std::string;

using namespace jpl::mipl::spice::corba;


static string s_fileName;

static void initFile()
{
   static bool s_hasBeenInitialized = false;

   if (s_hasBeenInitialized)
   {
      return;
   }

   ADS_TRACE;

   char buf[128];
   memset(buf, '\0', sizeof(buf));
   
   SpiceInt numVals = 0;
   SpiceBoolean wasFound = SPICEFALSE;
   gcpool_c("ADS_UPDATE_KERNEL",
            0,
            1,
            sizeof(buf),
            &numVals,
            buf,
            &wasFound);
   ads_checkForError();

   if ((wasFound != SPICETRUE) && (numVals != 1))
   {
      ACE_ERROR (( LM_ERROR, 
                   "ads_Database.cc: initFile: "
                   "Didn't find ADS_KERNEL_NAME.\n" ));
      ACE_OS::exit(1);
   }

   s_fileName = buf;

   furnsh_c(s_fileName.c_str());
   ads_checkForError();

   s_hasBeenInitialized = true;
}

static char* s_columnNames[] = 
{
   "time",
   "spacecraft_vector", 
   "solar_vector", 
   "planet_vector", 
   "spacecraft_velocity_vector", 
   "instrument_pointing", 
   "target_orientation", 
   "user", 
   "facility", 
   "application", 
   "notes"
};

static int numNames = sizeof(s_columnNames) / sizeof(char*);

string whereClause(string columnName, string value)
{
   string ret;
   if (value != "")
   {
      ret += " and ";
      ret += columnName;
      ret +=" eq '";
      ret += value;
      ret += "'";
   }
   return ret;
}

string buildQuery(const ImageData& image,
                  const GeometryMetaData& criteria)
{
   string query = "select ";
   for (int i=0; i < numNames; ++i)
   {
      if (i != 0)
      {
         query += ", ";
      }
      query += s_columnNames[i];
   }

   query += " from Geometry ";
   query += "where time eq '";
   query += image.utcTime.in();
   query += "'";

   string user = criteria.user.in();
   string facility = criteria.facility.in();
   string application = criteria.application.in();
   string notes = criteria.notes.in();

   query += whereClause("user", user);
   query += whereClause("facility", facility);
   query += whereClause("application", application);
   query += whereClause("notes", notes);

   return query;
}

void readDouble(int selectIndex, int row, SpDouble& d)
{
   ADS_TRACE;

   SpiceBoolean wasNull = SPICEFALSE;
   SpiceBoolean wasFound = SPICEFALSE;
   
   ekgd_c (selectIndex, row, 0, &d, &wasNull, &wasFound);
   ads_checkForError();
   
   if (wasNull == SPICETRUE)
   {  
      ACE_ERROR (( LM_ERROR, 
                   "ads_Database.cc: readDouble: "
                   "Error:  wasNull was true.\n" ));
      ACE_OS::exit(1);
   }

   if (wasFound == SPICEFALSE)
   {  
      ACE_ERROR (( LM_ERROR, 
                   "ads_Database.cc: readDouble: "
                   "Error:  wasFound was false.\n" ));
      ACE_OS::exit(1);
   }
}

void readVector(int selectIndex, int row, Vector3& v)
{
   ADS_TRACE;

   SpiceBoolean wasNull = SPICEFALSE;
   SpiceBoolean wasFound = SPICEFALSE;
   
   for (SpiceInt i = 0; i < 3; ++i)
   {
      ekgd_c (selectIndex, row, i, &v[i], &wasNull, &wasFound);
      ads_checkForError();
      
      if (wasNull == SPICETRUE)
      {  
         ACE_ERROR (( LM_ERROR, 
                      "ads_Database.cc: readVector: "
                      "Error:  wasNull was true.\n" ));
         ACE_OS::exit(1);
      }

      if (wasFound == SPICEFALSE)
      {  
         ACE_ERROR (( LM_ERROR, 
                      "ads_Database.cc: readVector: "
                      "Error:  wasFound was false.\n" ));
         ACE_OS::exit(1);
      }
   }
}


void readMatrix(int selectIndex, int row, Matrix33& m)
{
   ADS_TRACE;

   SpiceBoolean wasNull = SPICEFALSE;
   SpiceBoolean wasFound = SPICEFALSE;
   
   SpiceInt index = 0;
   for (SpiceInt i = 0; i < 3; ++i)
   {
      for (SpiceInt j = 0; j < 3; ++j, ++index)
      {
         ekgd_c (selectIndex, row, index, &m[i][j], &wasNull, &wasFound);
         ads_checkForError();

         if (wasNull == SPICETRUE)
         {  
            ACE_ERROR (( LM_ERROR, 
                         "ads_Database.cc: readMatrix: "
                         "Error:  wasNull was true.\n" ));
            ACE_OS::exit(1);
         }
         
         if (wasFound == SPICEFALSE)
         {  
            ACE_ERROR (( LM_ERROR, 
                         "ads_Database.cc: readMatrix: "
                         "Error:  wasFound was false.\n" ));
            ACE_OS::exit(1);
         }
      }
   }
}

const char* readString(int selectIndex, int row)
{
   ADS_TRACE;

   static char buf[128];
   memset(buf, '\0', sizeof(buf));

   SpiceBoolean wasNull = SPICEFALSE;
   SpiceBoolean wasFound = SPICEFALSE;
   
   ekgc_c (selectIndex, row, 0, sizeof(buf), buf, &wasNull, &wasFound);
   ads_checkForError();
   
   if (wasNull == SPICETRUE)
   {  
      ACE_ERROR (( LM_ERROR, 
                   "ads_Database.cc: readString: "
                   "Error:  wasNull was true.\n" ));
      ACE_OS::exit(1);
   }
   
   if (wasFound == SPICEFALSE)
   {  
      ACE_ERROR (( LM_ERROR, 
                   "ads_Database.cc: readString: "
                   "Error:  wasFound was false.\n" ));
      ACE_OS::exit(1);
   }

   return buf;
}








void writeDouble(SpiceInt handle, 
                 SpiceInt segmentNum, 
                 SpiceInt recordNum, 
                 int columnNum, 
                 double d)
{
   ADS_TRACE;

   ekaced_c ( handle, 
              segmentNum, 
              recordNum, 
              s_columnNames[columnNum], 
              sizeof(d)/sizeof(SpiceDouble), 
              &d, 
              SPICEFALSE);
   ads_checkForError();
}

void writeMatrix(SpiceInt handle, 
                 SpiceInt segmentNum, 
                 SpiceInt recordNum, 
                 int columnNum, 
                 const Matrix33& m)
{
   ADS_TRACE;

   ekaced_c ( handle, 
              segmentNum, 
              recordNum, 
              s_columnNames[columnNum], 
              sizeof(m)/sizeof(SpiceDouble), 
              m,
              SPICEFALSE);
   ads_checkForError();
}

void writeVector(SpiceInt handle, 
                 SpiceInt segmentNum, 
                 SpiceInt recordNum, 
                 int columnNum, 
                 const Vector3& v)
{
   ADS_TRACE;

   ekaced_c ( handle, 
              segmentNum, 
              recordNum, 
              s_columnNames[columnNum], 
              sizeof(v)/sizeof(SpiceDouble), 
              v, 
              SPICEFALSE);
   ads_checkForError();
}

void writeString(SpiceInt handle, 
                 SpiceInt segmentNum, 
                 SpiceInt recordNum, 
                 int columnNum, 
                 const char* s,
                 SpiceInt columnStringLen)
{
   ADS_TRACE;

   char* temp = new char[columnStringLen];
   memset(temp, '\0', columnStringLen);
   strcpy(temp, s);

   ekacec_c ( handle, 
              segmentNum, 
              recordNum, 
              s_columnNames[columnNum], 
              1,
              columnStringLen,
              temp,
              SPICEFALSE);

   delete[] temp;
   ads_checkForError();
}



bool ads_DataBase::findGeometry( const ImageData & image,
                                 const GeometryMetaData & criteria,
                                 Geometry_out theGeometry,
                                 GeometryMetaData_out metaData )
{
   ADS_TRACE;

   initFile();

   SpiceBoolean wasError = SPICEFALSE;
   char errStr[128];
   memset(errStr, '\0', sizeof(errStr));

   string query = buildQuery(image, criteria);
   SpiceInt numRowsFound = 0;
   ekfind_c(query.c_str(),
            sizeof(errStr),
            &numRowsFound,
            &wasError,
            errStr);
   ads_checkForError();

   if (wasError == SPICETRUE)
   {
       ACE_ERROR (( LM_ERROR, 
                   "ads_Database.cc: findGeometry: "
                   "Error in query string:[%s]\n", errStr ));
      ACE_OS::exit(1);     
   }

   if (numRowsFound < 1)
   {
      return false;
   }

   // If multiple rows are found, 
   // we'll return the last (and presumably newest) one.

   SpiceInt row = numRowsFound - 1;

   readVector(1, row, theGeometry.spacecraftVector);
   readVector(2, row, theGeometry.solarVector);
   readVector(3, row, theGeometry.planetVector);
   readVector(4, row, theGeometry.spacecraftVelocityVector);
   readMatrix(5, row, theGeometry.instrumentPointing);
   readMatrix(6, row, theGeometry.targetOrientation);
   metaData->user = readString(7, row);
   metaData->facility = readString(8, row);
   metaData->application  = readString(9, row);
   metaData->notes = readString(10, row);

   return true;
}


void ads_DataBase::storeGeometry( const ImageData & image,
                                  const Geometry & theGeometry,
                                  const GeometryMetaData & theMetaData )
{
   ADS_TRACE;

   initFile();

   // It's illegal to modify a loaded E kernel, 
   // so unload it now and reload it at the end.

   unload_c(s_fileName.c_str());
   ads_checkForError();

   SpiceInt handle = 0;
   ekopw_c ( s_fileName.c_str(), &handle );

   SpiceInt recordNum = 0;
   ekappr_c ( handle, 0, &recordNum );

   SpiceDouble time = 0.0;

   str2et_c(image.utcTime, &time);
   ads_checkForError();

   writeDouble(handle, 0, recordNum, 0, time);
   writeVector(handle, 0, recordNum, 1, theGeometry.spacecraftVector);
   writeVector(handle, 0, recordNum, 2, theGeometry.solarVector);
   writeVector(handle, 0, recordNum, 3, theGeometry.planetVector);
   writeVector(handle, 0, recordNum, 4, theGeometry.spacecraftVelocityVector);
   writeMatrix(handle, 0, recordNum, 5, theGeometry.instrumentPointing);
   writeMatrix(handle, 0, recordNum, 6, theGeometry.targetOrientation);

   // The database schema has columns of string length 32.
   SpiceInt len = 32;

   cout << "Write geometry for user '" << theMetaData.user.in() << "'" << endl;

   writeString(handle, 0, recordNum, 7, theMetaData.user.in(), len);
   writeString(handle, 0, recordNum, 8, theMetaData.facility.in(), len);
   writeString(handle, 0, recordNum, 9, theMetaData.application.in(), len);
   writeString(handle, 0, recordNum, 10, theMetaData.notes.in(), len);
   
   ekcls_c ( handle );
   ads_checkForError();

   furnsh_c(s_fileName.c_str());
   ads_checkForError();

}
