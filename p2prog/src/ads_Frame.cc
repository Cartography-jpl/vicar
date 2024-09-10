// ads_Frame.cc
//
// Apr 19, 2002
// Michael Brady

#include "ads_Frame.h"

#include "SpiceUsr.h"

ads_Frame::ads_Frame(int naifId)
   : m_naifId(naifId)
{
}

ads_Frame::~ads_Frame()
{
}

ads_Frame::ads_Frame(const ads_Frame& src)
   : m_naifId(src.m_naifId)
{
}

ads_Frame& ads_Frame::operator=(const ads_Frame& rhs)
{
   m_naifId = rhs.m_naifId;

   return *this;
}

int ads_Frame::naifId() const
{
   return m_naifId;
}

std::string ads_Frame::name() const
{
   SpiceChar name[128];
   memset(name, '\0', sizeof(name));
   
   frmnam_c ( m_naifId,
              sizeof(name),
              name );

   return std::string(name);
}
