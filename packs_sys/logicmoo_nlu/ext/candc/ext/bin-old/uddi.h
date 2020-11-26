// xsd:string schema type.
typedef char *xsd__string;
// xsd:boolean schema type.
typedef bool xsd__boolean;
// Dynamic array of strings.
struct	ArrayOfString
{	xsd__string	*__ptr;
	int		__size;
};
// Returns the first service access point under a service.
ns__FindServiceAccessPoint
(	xsd__string	strBusinessName,
	xsd__string	strServiceName,
	xsd__boolean	boolUseUDDITestSite,
	xsd__string	&FindServiceAccessPointResult
);
// Returns all service access points listed under a service.
ns__FindServiceAccessPoints
(	xsd__string		strBusinessName,
	xsd__string		strServiceName,
	xsd__boolean		boolUseUDDITestSite,
	struct ArrayOfString	&FindServiceAccessPointsResult
);
// Returns all service access points listed under a service whose description contains the query string.
ns__FindServiceAccessPointsQuery
(	xsd__string		strBusinessName,
	xsd__string		strServiceName,
	xsd__string		strQueryString,
	xsd__boolean		boolUseUDDITestSite,
	struct ArrayOfString	&FindServiceAccessPointsQueryResult
);
// Adds a service access point to a service. The service will be created if it does not exist.
ns__AddService
(	xsd__string	strBusinessName,
	xsd__string	strServiceName,
	xsd__string	strServiceDescription,
	xsd__string	strServiceAccessPoint,
	xsd__string	strServiceAccessPointDescription,
	xsd__string	strUDDIAdminLogin,
	xsd__string	strUDDIAdminPassword,
	xsd__boolean	boolUseUDDITestSite,
	xsd__boolean	&AddServiceResult
);
// Removes a service and all its access points.
ns__RemoveService
(	xsd__string	strBusinessName,
	xsd__string	strServiceName,
	xsd__string	strUDDIAdminLogin,
	xsd__string	strUDDIAdminPassword,
	xsd__boolean	boolUseUDDITestSite,
	xsd__boolean	&RemoveServiceResult
);
// Removes an access point from a service.
ns__RemoveServiceAccessPoint
(	xsd__string	strBusinessName,
	xsd__string	strServiceName,
	xsd__string	strServiceAccessPoint,
	xsd__string	strUDDIAdminLogin,
	xsd__string	strUDDIAdminPassword,
	xsd__boolean	boolUseUDDITestSite,
	xsd__boolean	&RemoveServiceAccessPointResult
);
