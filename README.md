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

# event_bot Usage
The event bot requires two files: one Linux/Unix pipe and an output file. For instance, the following shell commands start the bot with `input_pipe` and `output_file` as input and output.

```sh
mkfifo input_pipe
touch output_file
bin/event_bot myaccount@myhost.org ./input_pipe ./output_file
```

The `input_pipe` can receive any string to send to an XMPP client. If the message has the format `"to=AN_XMPP_JID MESSAGE"` then the message is sent to the JID provided. Else, the message is sent to myaccount@myhost.org. For example:

```
echo "to=bob@myhost.org hello Bob!" >> ./input_pipe
echo "hello myaccount!" >> ./input_pipe
```

The first command sends the message `hello Bob!` to bob@myhost.org and the second one sends `hello myaccount!` to myaccount@myhost.org.

The `output_file` is a text file with all the received chats from other XMPP clients. The message format is the following: `[DATE_IN_UTC] FULL_JID:MESSAGE`. For example:

```
[2021-06-02 12:29:53 UTC] bob@myhost.org/Conversations.12ab:Hello bot!
[2021-06-02 12:29:55 UTC] alice@myhost.org/Gajim.ab12:Hiiiii!
```

This file can be a FIFO pipe like the `input_pipe`, however it should be consulted frequently to avoid blocking. The bot get blocked when it is trying to write into a pipe without a reading process ready to consume the data. This is an operative system characteristic when usual file reading and writing instructions are used on pipes. Using a common file would not blocked the bot in any way. In fact, the file can be written or cleared as soon as a message is received because the bot closes it when idle.

# License
This work is under the General Public License version 3.
