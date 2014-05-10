
import os
import os.path

API = ['src/core/aws.ads',
       'src/core/aws-attachments.ads',
       'src/core/aws-client.ads',
       'src/extended/aws-client-hotplug.ads',
       'src/extended/aws-communication-client.ads',
       'src/extended/aws-communication-server.ads',
       'src/extended/aws-communication.ads',
       'src/core/aws-config-ini.ads',
       'src/core/aws-config-set.ads',
       'src/core/aws-config.ads',
       'src/core/aws-containers-tables.ads',
       'src/extended/aws-cookie.ads',
       'src/core/aws-default.ads',
       'src/core/aws-dispatchers-callback.ads',
       'src/core/aws-dispatchers.ads',
       'src/core/aws-exceptions.ads',
       'src/core/aws-headers.ads',
       'src/core/aws-headers-values.ads',
       'src/ldap/aws-ldap-client.ads',
       'src/core/aws-log.ads',
       'src/core/aws-messages.ads',
       'src/core/aws-mime.ads',
       'src/core/aws-net.ads',
       'src/core/aws-net-buffered.ads',
       'src/core/aws-net-log.ads',
       'src/extended/aws-net-log-callbacks.ads',
       'src/core/aws-net-ssl.ads',
       'src/core/aws-net-ssl-certificate.ads',
       'src/core/aws-net-websocket.ads',
       'src/core/aws-net-websocket-registry.ads',
       'src/core/aws-net-websocket-registry-control.ads',
       'src/core/aws-parameters.ads',
       'src/extended/aws-pop.ads',
       'src/core/aws-resources-files.ads',
       'src/core/aws-resources-embedded.ads',
       'src/core/aws-resources-streams.ads',
       'src/core/aws-resources-streams-disk.ads',
       'src/core/aws-resources-streams-disk-once.ads',
       'src/core/aws-resources-streams-memory.ads',
       'src/core/aws-resources-streams-memory-zlib.ads',
       'src/extended/aws-resources-streams-pipe.ads',
       'src/core/aws-resources.ads',
       'src/core/aws-response.ads',
       'src/extended/aws-server-hotplug.ads',
       'src/extended/aws-server-push.ads',
       'src/core/aws-server-status.ads',
       'src/core/aws-server-log.ads',
       'src/core/aws-server.ads',
       'src/extended/aws-services-callbacks.ads',
       'src/extended/aws-services-directory.ads',
       'src/extended/aws-services-dispatchers-linker.ads',
       'src/extended/aws-services-dispatchers-method.ads',
       'src/extended/aws-services-dispatchers-uri.ads',
       'src/extended/aws-services-dispatchers-virtual_host.ads',
       'src/extended/aws-services-dispatchers.ads',
       'src/extended/aws-services-download.ads',
       'src/extended/aws-services-page_server.ads',
       'src/extended/aws-services-split_pages.ads',
       'src/extended/aws-services-split_pages-uniform.ads',
       'src/extended/aws-services-split_pages-uniform-alpha.ads',
       'src/extended/aws-services-split_pages-uniform-overlapping.ads',
       'src/extended/aws-services-split_pages-alpha.ads',
       'src/extended/aws-services-split_pages-alpha-bounded.ads',
       'src/core/aws-services-transient_pages.ads',
       'src/extended/aws-services-web_block.ads',
       'src/extended/aws-services-web_block-context.ads',
       'src/extended/aws-services-web_block-registry.ads',
       'src/core/aws-session.ads',
       'src/extended/aws-smtp-client.ads',
       'src/extended/aws-smtp.ads',
       'src/core/aws-status.ads',
       'src/core/aws-templates.ads',
       'src/core/aws-translator.ads',
       'src/core/aws-url.ads',
       'src/xsrc/aws-jabber.ads',
       'src/soap/soap.ads',
       'src/soap/soap-client.ads',
       'src/soap/soap-dispatchers.ads',
       'src/soap/soap-dispatchers-callback.ads',
       'src/soap/soap-message-xml.ads',
       'src/soap/soap-message.ads',
       'src/soap/soap-parameters.ads',
       'src/soap/soap-types.ads']

if os.path.exists("../build/apirefs") is False:
    os.makedirs("../build/apirefs")

for path in API:
    try:
        fin = open("../../" + path)
        fout = open("../build/apirefs/" + os.path.basename(path), 'w')
        outside_private_part = True
        for line in fin:
            if line.startswith("end "):
                outside_private_part = True
            if outside_private_part:
                fout.write(line)
            if line == "private\n":
                outside_private_part = False
                fout.write("   -- implementation removed\n")
    finally:
        if fin:
            fin.close()
        if fout:
            fout.close()
