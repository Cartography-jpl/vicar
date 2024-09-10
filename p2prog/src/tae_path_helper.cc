#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
#include "vicmain_c.h"
#include <zvproto.h>
extern "C" {
#include <taeconf.inp>
#include <parblk.inc>
// We can't include taeintproto.h because it contains structures with 
// the reserved name "class".
void q_init(struct PARBLK *p, FUNINT pool_size, FUNINT mode);
FUNCTION CODE q_intg(struct PARBLK* p, const TEXT name[], FUNINT count, 
		     TAEINT intg[], FUNINT mode);
FUNCTION CODE q_real(struct PARBLK* p, const TEXT name[], FUNINT count, 
		     TAEFLOAT real[], FUNINT mode);
FUNCTION CODE q_string(struct PARBLK *p, const TEXT name[],
		       FUNINT count, const TEXT *vector[], FUNINT mode);
}

// This needs to match the value used in tae_path.pdf
const int max_string_len = 250;

void zmabendw(const std::string& V)
{
  zmabend(const_cast<char*>(V.c_str()));
}

// Copied from geocal vicar_argument class
void arg_type(const std::string& Keyword, std::string& Type, 
	      int& Count, int& Maxlen)
{
  int def;
  char tp[100];
  int status = zvpstat(const_cast<char*>(Keyword.c_str()), &Count, 
		       &def, &Maxlen, tp);
  if(status != 1)
    zmabendw("Call to zvpstat failed");
  Type = tp;
}

void arg_type(const std::string& Keyword, std::string& Type, 
	      int& Count)
{
  int maxlen;
  arg_type(Keyword, Type, Count, maxlen);
}

void arg_write_out(const std::string& Keyword, int Val)
{
  // This is copied directly from cartoTaeUtils.c. We have a copy here so
  // we don't need to link to that library.
  struct PARBLK parblk; 

  q_init(&parblk, 500, P_ABORT);
  q_intg(&parblk, Keyword.c_str(), 1, &Val, P_ADD);
  zvq_out(&parblk);
}

void arg_write_out(const std::string& Keyword, double Val)
{
  // This is copied directly from cartoTaeUtils.c. We have a copy here so
  // we don't need to link to that library.
  struct PARBLK parblk; 

  q_init(&parblk, 500, P_ABORT);
  q_real(&parblk, Keyword.c_str(), 1, &Val, P_ADD);
  zvq_out(&parblk);
}

void arg_write_out(const std::string& Keyword, const std::string& Val,
		   int Max_len = max_string_len)
{
  // This is copied directly from cartoTaeUtils.c. We have a copy here so
  // we don't need to link to that library.
  struct PARBLK parblk; 

  q_init(&parblk, 500, P_ABORT);
  if((int) Val.size() > Max_len - 1)
    zmabendw("string value is too long");
  const char* v[1];
  v[0] = Val.c_str();
  q_string(&parblk, Keyword.c_str(), 1, v, P_ADD);
  zvq_out(&parblk);
}

template<class T> T vicar_arg(const std::string& Keyword);

template<> inline int vicar_arg(const std::string& Keyword)
{
  std::string tp("b");		// Mac is flaky passing empty strings
				// around between shared
				// libraries. 
  int count;
  arg_type(Keyword, tp, count);
  if(count != 1)
    zmabendw("Count !=1");
  if(tp != "INT")
    zmabendw("Type '" + tp + "' is not INT");
  int res;
  int status = zvp(const_cast<char*>(Keyword.c_str()), &res, &count);
  if(status != 1)
    zmabendw("Call to zvp failed");
  return res;
}


void main44(void)
{
  std::string s = "";
  if(getenv("TAE_PATH"))
    s = std::string(getenv("TAE_PATH"));
  std::vector<std::string> p;
  size_t spos = 0;
  while(spos != std::string::npos) {
    size_t npos = s.find(':', spos);
    if(npos == std::string::npos) {
      std::string t = s.substr(spos);
      if(t != "")
	p.push_back(t);
      spos = npos;
    } else {
      std::string t = s.substr(spos, npos - spos);
      if(t != "")
	p.push_back(t);
      spos = npos + 1;
    }
  }
  int index = vicar_arg<int>("index");
  if(index < 0 || index >= (int) p.size())
    arg_write_out("value", std::string("---"));
  else
    arg_write_out("value", p[index]);
}
