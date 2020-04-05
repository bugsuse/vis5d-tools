Summary: grib2v5d: Converter from grib to Vis5d file format
Name: grib2v5d
Version: 2.6
Release: 1%{dist}
License: GPL
Group: Meteo
URL: http://grib2v5d.sf.net/
Packager: Davide Cesari <cesari@users.sf.net>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot

%description
The program grib2v5d converts the content of a grib file (a format
used for exchanging meteorological model fields) into a file that can
be viewed with Vis5d, a popular program for 3-D visualisation of
meteorological fields.

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
%{_docdir}/%{name}/*

%clean
rm -rf %{buildroot}

%pre

%post

%preun

%postun

%changelog
* Fri Jul 01 2005 Davide Cesari <cesari@users.sf.net> 2.3-1
- First version with autoconf/automake
