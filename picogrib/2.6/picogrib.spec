Summary: picogrib: GRIB decoding library compatible with ECMWF EMOS library
Name: picogrib
Version: 2.6
Release: 1
License: GPL
Group: Meteo
URL: http://picogrib.sf.net/
Packager: Davide Cesari <cesari@users.sf.net>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot

%description
Picogrib is a multi-platform, C-language, FORTRAN callable, GRIB decoding
package, compatible to some extent with ECMWF EMOS library.


%prep
%setup -q

%build
%configure
make 

%install
rm -rf %{buildroot}
make DESTDIR=%{buildroot} install


%files
%defattr(-, root, root)
%{_bindir}/*
%{_includedir}/*.h
%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so*
%{_docdir}/%{name}/*

%clean
rm -rf %{buildroot}

%pre

%post

%preun

%postun

%changelog
* Mon Dec 10 2007 Davide Cesari <cesari@arpa.emr.it> - 2.6-2
- Fixed makeinstall for Fedora 8.

* Fri Feb 23 2007 Davide Cesari <cesari@users.sf.net> - 2.6-1
- New version

* Mon Nov 28 2005 Davide Cesari <cesari@users.sf.net> - 2.5-2
- Additional formats in partgrib and triangular grid support

* Fri Jul 01 2005 Davide Cesari <cesari@users.sf.net> - 2.5-1
- First version with autoconf/automake
