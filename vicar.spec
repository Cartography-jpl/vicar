Summary: This is the VICAR programs, including taetm
Name: vicar
Version: 1.13.1
Release: 1.el%{rhel}
License: Copyright 2022 California Institute of Technology ALL RIGHTS RESERVED
Group: Applications/Engineering
Vendor: California Institute of Technology
URL: http://www-mipl.jpl.nasa.gov/external/vicar.html
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: vicar-rtl ncurses-devel
Prefix: /opt/afids_support

%description

This is the VICAR p2 programs, along with the taetm (the VICAR shell).

%prep
%setup -q

%build
./configure --prefix=/opt/afids_support 
make %_smp_mflags 

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc
/opt/afids_support/vicar/bin/*
/opt/afids_support/vicar/vicset1.csh
/opt/afids_support/vicar/vicset2.csh
/opt/afids_support/vicar/tae/bin/taetm
/opt/afids_support/vicar/tae/bin/all/configfile  
/opt/afids_support/vicar/tae/bin/all/cversion  
/opt/afids_support/vicar/tae/bin/all/forcomp  
/opt/afids_support/vicar/tae/bin/all/fversion  
/opt/afids_support/vicar/tae/bin/all/platform  
/opt/afids_support/vicar/tae/bin/all/taeccone  
/opt/afids_support/vicar/tae/bin/all/tae.mkmf
/opt/afids_support/vicar/tae/bin/csh/taesetup  
/opt/afids_support/vicar/tae/bin/csh/taesetupclassic  
/opt/afids_support/vicar/tae/bin/csh/taesetupGen.pl  
/opt/afids_support/vicar/tae/bin/csh/taesetupmin  
/opt/afids_support/vicar/tae/bin/csh/taesetupsrc
/opt/afids_support/vicar/tae/help/msg/taefac.msg
/opt/afids_support/vicar/tae/pdf/async.pdf
/opt/afids_support/vicar/tae/pdf/batch.pdf
/opt/afids_support/vicar/tae/pdf/convert.pdf
/opt/afids_support/vicar/tae/pdf/ex.pdf
/opt/afids_support/vicar/tae/pdf/gp.pdf
/opt/afids_support/vicar/tae/pdf/msgbld.pdf
/opt/afids_support/vicar/tae/pdf/show.pdf
/opt/afids_support/vicar/tae/pdf/slogoff.pdf
/opt/afids_support/vicar/tae/pdf/slogon.pdf
/opt/afids_support/vicar/tae/pdf/taegbl.pdf
/opt/afids_support/vicar/tae/pdf/tae_path.pdf
/opt/afids_support/vicar/tae/pdf/tae_path_helper
/opt/afids_support/vicar/tae/pdf/tae_path_helper.pdf
/opt/afids_support/vicar/tae/pdf/ulogoff.pdf
/opt/afids_support/vicar/tae/pdf/ulogon.pdf
/opt/afids_support/vicar/tae/pdf/wbcolors.pdf
/opt/afids_support/vicar/tae/pdf/wbfonts.pdf
/opt/afids_support/vicar/tae/menu/tests.mdf
/opt/afids_support/vicar/tae/menu/root.mdf
/opt/afids_support/vicar/tae/menu/util.mdf
/opt/afids_support/vicar/tae/menu/demo.mdf
/opt/afids_support/man/man1/taeccone.1  
/opt/afids_support/man/man1/taetm.1  
/opt/afids_support/man/man1/TAE_tools.1
/opt/afids_support/man/man3/Co_.3
/opt/afids_support/man/man3/Co.3
/opt/afids_support/man/man3/Co_Add.3
/opt/afids_support/man/man3/Co_Find.3
/opt/afids_support/man/man3/Co_ForEach.3
/opt/afids_support/man/man3/Co_Free.3
/opt/afids_support/man/man3/Co_Intro.3
/opt/afids_support/man/man3/Co_New.3
/opt/afids_support/man/man3/Co_ReadFile.3
/opt/afids_support/man/man3/Co_Remove.3
/opt/afids_support/man/man3/Co_WriteFile.3
/opt/afids_support/man/man3/f_force_lower.3
/opt/afids_support/man/man3/s_and_f_pkgs.3
/opt/afids_support/man/man3/s_equal.3
/opt/afids_support/man/man3/tae_alloc.3
/opt/afids_support/man/man3/tae_free.3
/opt/afids_support/man/man3/t_pinit.3
/opt/afids_support/man/man3/Vm_.3
/opt/afids_support/man/man3/Vm.3
/opt/afids_support/man/man3/Vm_Call.3
/opt/afids_support/man/man3/Vm_Copy.3
/opt/afids_support/man/man3/Vm_DynTutor.3
/opt/afids_support/man/man3/Vm_Find.3
/opt/afids_support/man/man3/Vm_FindVar.3
/opt/afids_support/man/man3/Vm_ForEach.3
/opt/afids_support/man/man3/Vm_FormatVar.3
/opt/afids_support/man/man3/Vm_Free.3
/opt/afids_support/man/man3/Vm_GetAttribute.3
/opt/afids_support/man/man3/Vm_GetHostError.3
/opt/afids_support/man/man3/Vm_GetValidIntg.3
/opt/afids_support/man/man3/Vm_GetValidReal.3
/opt/afids_support/man/man3/Vm_GetValidString.3
/opt/afids_support/man/man3/Vm_InitFormat.3
/opt/afids_support/man/man3/Vm_Intro.3
/opt/afids_support/man/man3/Vm_New.3
/opt/afids_support/man/man3/Vm_OpenStdout.3
/opt/afids_support/man/man3/Vm_ReadFromDisk.3
/opt/afids_support/man/man3/Vm_ReadFromTM.3
/opt/afids_support/man/man3/Vm_SetIntg.3
/opt/afids_support/man/man3/Vm_SetMax.3
/opt/afids_support/man/man3/Vm_SetMin.3
/opt/afids_support/man/man3/Vm_SetNextMenu.3
/opt/afids_support/man/man3/Vm_SetParmPage.3
/opt/afids_support/man/man3/Vm_SetReal.3
/opt/afids_support/man/man3/Vm_SetString.3
/opt/afids_support/man/man3/Vm_SetStringLength.3
/opt/afids_support/man/man3/Vm_SetTCLVar.3
/opt/afids_support/man/man3/Vm_SetValidIntg.3
/opt/afids_support/man/man3/Vm_SetValidReal.3
/opt/afids_support/man/man3/Vm_SetValidString.3
/opt/afids_support/man/man3/Vm_WriteToDisk.3
/opt/afids_support/man/man5/wptcloseitems.5
/opt/afids_support/vicar/tae/help/tm/delcmd.hlp
/opt/afids_support/vicar/tae/help/tm/nscdqltut.hlp
/opt/afids_support/vicar/tae/help/tm/newcmode.hlp
/opt/afids_support/vicar/tae/help/tm/subcmd.hlp
/opt/afids_support/vicar/tae/help/tm/setlib.hlp
/opt/afids_support/vicar/tae/help/tm/alloc.hlp
/opt/afids_support/vicar/tae/help/tm/reply.hlp
/opt/afids_support/vicar/tae/help/tm/dismount.hlp
/opt/afids_support/vicar/tae/help/tm/commode.hlp
/opt/afids_support/vicar/tae/help/tm/mount.hlp
/opt/afids_support/vicar/tae/help/tm/goto.hlp
/opt/afids_support/vicar/tae/help/tm/menumode.hlp
/opt/afids_support/vicar/tae/help/tm/wait.hlp
/opt/afids_support/vicar/tae/help/tm/restore.hlp
/opt/afids_support/vicar/tae/help/tm/exit.hlp
/opt/afids_support/vicar/tae/help/tm/deflog.hlp
/opt/afids_support/vicar/tae/help/tm/sendvar.hlp
/opt/afids_support/vicar/tae/help/tm/view.hlp
/opt/afids_support/vicar/tae/help/tm/process.hlp
/opt/afids_support/vicar/tae/help/tm/loop.hlp
/opt/afids_support/vicar/tae/help/tm/logoff.hlp
/opt/afids_support/vicar/tae/help/tm/dealloc.hlp
/opt/afids_support/vicar/tae/help/tm/local.hlp
/opt/afids_support/vicar/tae/help/tm/nscpqltut.hlp
/opt/afids_support/vicar/tae/help/tm/remove.hlp
/opt/afids_support/vicar/tae/help/tm/switch.hlp
/opt/afids_support/vicar/tae/help/tm/return.hlp
/opt/afids_support/vicar/tae/help/tm/prmtmode.hlp
/opt/afids_support/vicar/tae/help/tm/break.hlp
/opt/afids_support/vicar/tae/help/tm/display.hlp
/opt/afids_support/vicar/tae/help/tm/emit.hlp
/opt/afids_support/vicar/tae/help/tm/usedisp.hlp
/opt/afids_support/vicar/tae/help/tm/enable.hlp
/opt/afids_support/vicar/tae/help/tm/tutor.hlp
/opt/afids_support/vicar/tae/help/tm/stop.hlp
/opt/afids_support/vicar/tae/help/tm/fmtdyntut.hlp
/opt/afids_support/vicar/tae/help/tm/fmtsubtut.hlp
/opt/afids_support/vicar/tae/help/tm/if.hlp
/opt/afids_support/vicar/tae/help/tm/flag.hlp
/opt/afids_support/vicar/tae/help/tm/abort.hlp
/opt/afids_support/vicar/tae/help/tm/defcmd.hlp
/opt/afids_support/vicar/tae/help/tm/menu.hlp
/opt/afids_support/vicar/tae/help/tm/defsym.hlp
/opt/afids_support/vicar/tae/help/tm/fmtdqltut.hlp
/opt/afids_support/vicar/tae/help/tm/window.hlp
/opt/afids_support/vicar/tae/help/tm/freedisp.hlp
/opt/afids_support/vicar/tae/help/tm/nscprmtut.hlp
/opt/afids_support/vicar/tae/help/tm/parmset.hlp
/opt/afids_support/vicar/tae/help/tm/attach.hlp
/opt/afids_support/vicar/tae/help/tm/globals.hlp
/opt/afids_support/vicar/tae/help/tm/putmsg.hlp
/opt/afids_support/vicar/tae/help/tm/delete.hlp
/opt/afids_support/vicar/tae/help/tm/intmode.hlp
/opt/afids_support/vicar/tae/help/tm/body.hlp
/opt/afids_support/vicar/tae/help/tm/compile.hlp
/opt/afids_support/vicar/tae/help/tm/continue.hlp
/opt/afids_support/vicar/tae/help/tm/rewind.hlp
/opt/afids_support/vicar/tae/help/tm/procedure.hlp
/opt/afids_support/vicar/tae/help/tm/nscsubtut.hlp
/opt/afids_support/vicar/tae/help/tm/fmtpqltut.hlp
/opt/afids_support/vicar/tae/help/tm/parm.hlp
/opt/afids_support/vicar/tae/help/tm/show.hlp
/opt/afids_support/vicar/tae/help/tm/nscdyntut.hlp
/opt/afids_support/vicar/tae/help/tm/wpt.hlp
/opt/afids_support/vicar/tae/help/tm/fmtvalsel.hlp
/opt/afids_support/vicar/tae/help/tm/write.hlp
/opt/afids_support/vicar/tae/help/tm/dal.hlp
/opt/afids_support/vicar/tae/help/tm/let.hlp
/opt/afids_support/vicar/tae/help/tm/emacs.hlp
/opt/afids_support/vicar/tae/help/tm/next.hlp
/opt/afids_support/vicar/tae/help/tm/refgbl.hlp
/opt/afids_support/vicar/tae/help/tm/ush.hlp
/opt/afids_support/vicar/tae/help/tm/for.hlp
/opt/afids_support/vicar/tae/help/tm/getpar.hlp
/opt/afids_support/vicar/tae/help/tm/save.hlp
/opt/afids_support/vicar/tae/help/tm/mal.hlp
/opt/afids_support/vicar/tae/help/tm/recvar.hlp
/opt/afids_support/vicar/tae/help/tm/disable.hlp
/opt/afids_support/vicar/tae/help/tm/fmtprmtut.hlp
/opt/afids_support/vicar/tae/help/tm/help.hlp
/opt/afids_support/share/fonts/vrdi/000.fon
/opt/afids_support/share/fonts/vrdi/001.fon
/opt/afids_support/share/fonts/vrdi/002.fon
/opt/afids_support/share/fonts/vrdi/003.fon
/opt/afids_support/share/fonts/vrdi/004.fon
/opt/afids_support/share/fonts/vrdi/005.fon
/opt/afids_support/share/fonts/vrdi/006.fon
/opt/afids_support/share/fonts/vrdi/007.fon
/opt/afids_support/share/fonts/vrdi/008.fon
/opt/afids_support/share/fonts/vrdi/009.fon
/opt/afids_support/share/fonts/vrdi/010.fon
/opt/afids_support/share/fonts/vrdi/011.fon
/opt/afids_support/share/fonts/vrdi/012.fon
/opt/afids_support/share/fonts/vrdi/013.fon
/opt/afids_support/share/fonts/vrdi/014.fon
/opt/afids_support/share/fonts/vrdi/020.fon
/opt/afids_support/share/fonts/vrdi/030.fon
/opt/afids_support/share/fonts/vrdi/102.fon
/opt/afids_support/share/fonts/vrdi/103.fon
/opt/afids_support/share/fonts/vrdi/104.fon
/opt/afids_support/share/fonts/vrdi/105.fon
/opt/afids_support/share/fonts/vrdi/106.fon
/opt/afids_support/share/fonts/vrdi/107.fon
/opt/afids_support/share/fonts/vrdi/111.fon
/opt/afids_support/share/fonts/vrdi/112.fon
/opt/afids_support/share/fonts/vrdi/113.fon
/opt/afids_support/share/fonts/vrdi/114.fon
/opt/afids_support/share/fonts/vrdi/115.fon
/opt/afids_support/share/fonts/vrdi/116.fon
/opt/afids_support/share/fonts/vrdi/117.fon
/opt/afids_support/share/fonts/vrdi/118.fon
/opt/afids_support/share/fonts/vrdi/119.fon
/opt/afids_support/share/fonts/vrdi/120.fon
/opt/afids_support/share/fonts/vrdi/121.fon
/opt/afids_support/share/fonts/vrdi/122.fon
/opt/afids_support/share/fonts/vrdi/123.fon
/opt/afids_support/share/fonts/vrdi/124.fon
/opt/afids_support/share/fonts/vrdi/125.fon
/opt/afids_support/share/fonts/vrdi/126.fon
/opt/afids_support/share/fonts/vrdi/127.fon
/opt/afids_support/share/fonts/vrdi/128.fon
/opt/afids_support/share/fonts/vrdi/129.fon
/opt/afids_support/share/fonts/vrdi/130.fon
/opt/afids_support/share/fonts/vrdi/131.fon
/opt/afids_support/share/fonts/vrdi/203.fon
/opt/afids_support/vicar/lib/request.pdf
/opt/afids_support/vicar/lib/showdisp.pdf
/opt/afids_support/vicar/lib/syntax.pdf
/opt/afids_support/vicar/lib/v2version.pdf
/opt/afids_support/vicar/lib/vic2fac.inx
/opt/afids_support/vicar/lib/vic2fac.msg
/opt/afids_support/vicar/lib/vicar.pdf
/opt/afids_support/vicar/lib/vrdifac.inx
/opt/afids_support/vicar/lib/vrdifac.msg
/opt/afids_support/vicar/vids/lib/chkvids1.cpd
/opt/afids_support/vicar/vids/lib/chkvids1.pdf
/opt/afids_support/vicar/vids/lib/chkvids2.cpd
/opt/afids_support/vicar/vids/lib/chkvids2.pdf
/opt/afids_support/vicar/vids/lib/chkvids3.cpd
/opt/afids_support/vicar/vids/lib/chkvids3.pdf
/opt/afids_support/vicar/vids/lib/jblink.cpd
/opt/afids_support/vicar/vids/lib/jblink.pdf
/opt/afids_support/vicar/vids/lib/jbw.cpd
/opt/afids_support/vicar/vids/lib/jbw.pdf
/opt/afids_support/vicar/vids/lib/jcolor.cpd
/opt/afids_support/vicar/vids/lib/jcolor.pdf
/opt/afids_support/vicar/vids/lib/jcursor.cpd
/opt/afids_support/vicar/vids/lib/jcursor.pdf
/opt/afids_support/vicar/vids/lib/jdef.cpd
/opt/afids_support/vicar/vids/lib/jdef.pdf
/opt/afids_support/vicar/vids/lib/jdisp.cpd
/opt/afids_support/vicar/vids/lib/jdisp.pdf
/opt/afids_support/vicar/vids/lib/jdraw.cpd
/opt/afids_support/vicar/vids/lib/jdraw.pdf
/opt/afids_support/vicar/vids/lib/jdrop.cpd
/opt/afids_support/vicar/vids/lib/jdrop.pdf
/opt/afids_support/vicar/vids/lib/jdtf.cpd
/opt/afids_support/vicar/vids/lib/jdtf.pdf
/opt/afids_support/vicar/vids/lib/jerase.cpd
/opt/afids_support/vicar/vids/lib/jerase.pdf
/opt/afids_support/vicar/vids/lib/jget.cpd
/opt/afids_support/vicar/vids/lib/jget.pdf
/opt/afids_support/vicar/vids/lib/jgrab.cpd
/opt/afids_support/vicar/vids/lib/jgrab.pdf
/opt/afids_support/vicar/vids/lib/jgraphics.cpd
/opt/afids_support/vicar/vids/lib/jgraphics.pdf
/opt/afids_support/vicar/vids/lib/jgrdisp.cpd
/opt/afids_support/vicar/vids/lib/jgrdisp.pdf
/opt/afids_support/vicar/vids/lib/jhist.cpd
/opt/afids_support/vicar/vids/lib/jhist.pdf
/opt/afids_support/vicar/vids/lib/jlist.cpd
/opt/afids_support/vicar/vids/lib/jlist.pdf
/opt/afids_support/vicar/vids/lib/jload.cpd
/opt/afids_support/vicar/vids/lib/jload.pdf
/opt/afids_support/vicar/vids/lib/jmovie.cpd
/opt/afids_support/vicar/vids/lib/jmovie.pdf
/opt/afids_support/vicar/vids/lib/joff.cpd
/opt/afids_support/vicar/vids/lib/joff.pdf
/opt/afids_support/vicar/vids/lib/jon.cpd
/opt/afids_support/vicar/vids/lib/jon.pdf
/opt/afids_support/vicar/vids/lib/jpan.cpd
/opt/afids_support/vicar/vids/lib/jpan.pdf
/opt/afids_support/vicar/vids/lib/jphoto.cpd
/opt/afids_support/vicar/vids/lib/jphoto.pdf
/opt/afids_support/vicar/vids/lib/jprofile.cpd
/opt/afids_support/vicar/vids/lib/jprofile.pdf
/opt/afids_support/vicar/vids/lib/jpscopy.cpd
/opt/afids_support/vicar/vids/lib/jpscopy.pdf
/opt/afids_support/vicar/vids/lib/jpsedit.cpd
/opt/afids_support/vicar/vids/lib/jpsedit.pdf
/opt/afids_support/vicar/vids/lib/jpseudo.cpd
/opt/afids_support/vicar/vids/lib/jpseudo.pdf
/opt/afids_support/vicar/vids/lib/jsave.cpd
/opt/afids_support/vicar/vids/lib/jsave.pdf
/opt/afids_support/vicar/vids/lib/jset.cpd
/opt/afids_support/vicar/vids/lib/jset.pdf
/opt/afids_support/vicar/vids/lib/jshow.cpd
/opt/afids_support/vicar/vids/lib/jshow.pdf
/opt/afids_support/vicar/vids/lib/jstats.cpd
/opt/afids_support/vicar/vids/lib/jstats.pdf
/opt/afids_support/vicar/vids/lib/jstop.cpd
/opt/afids_support/vicar/vids/lib/jstop.pdf
/opt/afids_support/vicar/vids/lib/jstretch.cpd
/opt/afids_support/vicar/vids/lib/jstretch.pdf
/opt/afids_support/vicar/vids/lib/jtext.cpd
/opt/afids_support/vicar/vids/lib/jtext.pdf
/opt/afids_support/vicar/vids/lib/jwedge.cpd
/opt/afids_support/vicar/vids/lib/jwedge.pdf
/opt/afids_support/vicar/vids/lib/jzoom.cpd
/opt/afids_support/vicar/vids/lib/jzoom.pdf
/opt/afids_support/vicar/vids/lib/planenames.cpd
/opt/afids_support/vicar/vids/lib/planenames.pdf
/opt/afids_support/vicar/vids/lib/vids.cpd
/opt/afids_support/vicar/vids/lib/vids.pdf
/opt/afids_support/vicar/vids/lib/vidsglob.cpd
/opt/afids_support/vicar/vids/lib/vidsglob.pdf
/opt/afids_support/vicar/vids/lib/vidslogon.cpd
/opt/afids_support/vicar/vids/lib/vidslogon.pdf
/opt/afids_support/vicar/vids/lib/vidsprog.cpd
/opt/afids_support/vicar/vids/lib/vidsprog.pdf
/opt/afids_support/vicar/vids/lib/vidsversion.cpd
/opt/afids_support/vicar/vids/lib/vidsversion.pdf
%changelog
* Thu Jun 9 2022 Smyth <smyth@macsmyth> - 1.13-1.el%{rhel}
- Added shebang to various scripts.

* Wed Sep 25 2019 Smyth <smyth@macsmyth> - 1.12-1.el%{rhel}
- Add float support to geomv.

* Fri Jul 20 2018 Smyth <smyth@macsmyth> - 1.11-2.el%{rhel}
- Rebuild after changes to vicar_rtl

* Fri Oct 7 2016 Mike M Smyth <smyth@pistol> - 1.11-1.el%{rhel}
- Remove a few programs that are now in afids vdev.

* Tue May 17 2016 Mike M Smyth <smyth@pistol> - 1.10-1.el%{rhel}
- Update to use TAE_PATH instead of older R1LIB etc. Also add support
  for having "-" in the directory names.

* Thu Dec 17 2015 Mike M Smyth <smyth@pistol> - 1.09-2
- Rebuild

* Wed Nov 25 2015 Mike M Smyth <smyth@pistol> - 1.09-1
- Minor changes in vicset1.csh file

* Thu May 29 2014 Mike M Smyth <smyth@pistol> - 1.08-1
- Fix a buffer size bug in ibis_copy

* Fri Feb 21 2014 Mike M Smyth <smyth@pistol> - 1.07-1
- Fix a number of bugs in ibis_copy

* Tue Sep 24 2013 Mike M Smyth <smyth@pistol> - 1.06-1
- Remove pixstar and fftflip, which is now done in AFIDS with Ray's 
  improvements.

* Wed Sep  4 2013 Mike M Smyth <smyth@pistol> - 1.05-1
- Remove the programs addnoise, addtofil, apodize, average, boxflt2,
  ccomp, edibis, fitsin, img2ascii, imgstat, insect, lab2tcl, minfilt,
  rotate2, spot. Ray will be supplying updated versions of these files 
  in the Afids area.

* Fri Aug 16 2013 Mike M Smyth <smyth@pistol> - 1.04-1
- Remove gausnois ltgen maxmin nxt from building. These will be replaced 
  with programs in afids vdev.

* Fri Aug  9 2013 Mike M Smyth <smyth@pistol> - 1.03-1
- Add in vpack

* Thu Jun 27 2013 Mike M Smyth <smyth@pistol> - 1.02-1
- Fix a mistake in the last change

* Tue Jun 25 2013 Mike M Smyth <smyth@pistol> - 1.01-1
- Remove setting V2DATA environment variable, which is now done in afids_xvd

* Mon Nov 26 2012 Mike M Smyth <smyth@pistol> - 
- Initial build.

