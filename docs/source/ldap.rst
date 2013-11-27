.. _LDAP:

****
LDAP
****

.. index:: LDAP
.. index:: LDAP Directory
.. index:: Lightweight Directory Access Protocol

`AWS` provides a complete API to retrieve information from LDAP servers.
Note that there is no support for updating, modifying or deleting
information only to read information from the server.

The `AWS/LDAP` implementation is based on `OpenLDAP`. To
build an LDAP application you need to link with the :file:`libldap.a`
library. This library is built by `AWS` on Windows based system
and will use the :file:`wldap32.dll` as provided with Windows
NT/2000/XP. On UNIX based systems, you must install properly the
`OpenLDAP` package.

The steps required to read information from an LDAP server are:

.. highlight:: ada

*Initialize the LDAP directory*
  We open a connection::

     declare
       Directory : LDAP.Client.Directory;
     begin
       Directory := LDAP.Client.Init (Host);

  Host is the hostname where the LDAP directory is running. It is
  possible to specify the port if the LDAP server does not use the
  default one.

*Bind to the LDAP server*
  This step is the way to pass a login/password if the LDAP server
  required an authentication. If not, the login/password must be empty strings::

    LDAP.Client.Bind (Directory, "", "");

*Do the search*
  For the search you must specify the base name, a filter, the scope and
  a set of attributes to retrieve::

    Response_Set := LDAP.Client.Search
      (Directory, Base_DN, Filter, LDAP.Client.LDAP_Scope_Subtree,
    LDAP.Client.Attributes ("cn", "sn", "telephonenumber"));

*Attributes*
    The set of attributes to retrieve from the directory.

*Filter*
    A set of values for some attributes. A filter is <attribute_name>=<value>
    where value can contain '*' at the end. For example "(cn=DUPON*)" will
    look for all entries where the common name is starting by the
    string "DUPON".

*Scope*
    Define how far in the hierarchical directory the search will
    operate. It is either one level, all subtrees or on the base of the tree.

  For more information see :ref:`AWS.LDAP.Client`.

*Iterate through the response set*
  For this there is two iterators. `First_Entry`/`Next_Entry` or the
  generic high level iterator `For_Every_Entry`::

    declare
      Message : LDAP.Client.LDAP_Message;
    begin
      Message := LDAP.Client.First_Entry (Directory, Response_Set);

      while Message /= LDAP.Client.Null_LDAP_Message loop
        Do_Job (Message);

        Message := LDAP.Client.Next_Entry (Directory, Message);
      end loop;
    end;

*Read attributes for each entry*
  Each entry has an associated set of attributes. To retrieve attributes
  values there is two iterators. `First_Attribute` / `Next_Attribute`
  or the generic high level iterator `For_Every_Attribute`::

    declare
      BER  : aliased LDAP.Client.BER_Element;
      Attr : constant String := LDAP.Client.First_Attribute
               (Directory, Message, BER'Unchecked_Access);
    begin
      Do_Job (Attr);

      loop
        declare
          Attr : constant String := LDAP.Client.Next_Attribute
                   (Directory, Message, BER);
        begin
          exit when Attr = "";
          Do_Job (Attr);
        end;
      end loop;
    end;

*Cleanup*
  At the end of the processing it is important to release memory
  associated with LDAP objects::

    LDAP.Client.Free (Message);
    LDAP.Client.Unbind (Directory);

See :ref:`AWS.LDAP.Client` for all high level supported API and documentation.

Note that for complete information about `AWS/LDAP` you you should read
an LDAP API description. `AWS/LDAP` is only a binding and follow the
LDAP API closely.
