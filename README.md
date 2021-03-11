# AXMPP Utils
Bots and tools created using the AXMPP library. 

Requisites: 

- [AXMPP library](https://github.com/cnngimenez/axmpp) ([see original fork](https://github.com/coopht/axmpp))
- [Matreshka libraries](http://forge.ada-ru.org/matreshka/)
- [Ada Utils](https://github.com/stcarrez/ada-util/) (required for the HTTP Upload when sharing files)
- GNAT compiler.
- GPRBuild.

# References

- [XMPP.org](https://xmpp.org/)
- [RFC6120 "XMPP Core"](https://www.rfc-editor.org/rfc/rfc6120.html)
- [RFC6121 "XMPP Instant Messaging and Presence"](https://www.rfc-editor.org/rfc/rfc6121.html)

# Compiling

Install AXMPP first. Remember to set `ADA_PROJECT_PATH`, `ADA_INCLUDE_PATH` and other environment variables accordingly. Also, `LD_LIBRARY_PATH` must point to the *.so.

Compilation start by typing `make`. 

# Configuration
Create a text file `config/connection.cfg` with the following data:

- first line: JID (without resource)
- second line: username
- third line: host
- fourth line: password
- fifth line: resource name

For example, this content is a valid configuration:

```
mybot@myhost.org
mybot
myhost.org
mybotpassword
axmpp_1
```
