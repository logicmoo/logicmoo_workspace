class TWhoIsInfo
{ public:
  char *Registrar;
  char *WhoIsServer;
  char *ReferralURL;
  char *NameServer1;
  char *NameServer2;
  char *UpdateDate;
  char *LastUpdateDate;
};
struct getWhoIsResponse {TWhoIsInfo *ns1__return, *ns2__return;};
int getWhoIs(char *ADomainName, struct getWhoIsResponse *result);
