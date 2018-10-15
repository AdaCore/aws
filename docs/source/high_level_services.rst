.. _High_level_services:

.. highlight:: ada

*******************
High level services
*******************

Here you will find a description of high level services. These services are
ready to use with AWS and can be used together with user's callbacks.

Refer to the Ada spec for a complete API and usage description.

.. _Directory_browser:

Directory browser
=================

.. index:: Directory browser

This service will help building a Web directory browser. It has a lot
of options to sort directory entries and is based on the templates
interface :ref:`AWS.Templates`. This means that you can use the
default directory template or provide your own.

see :ref:`AWS.Services.Directory` for complete spec and services descriptions.

.. _Dispatchers:

Dispatchers
===========

.. index:: Dispatchers

In many AWS applications it is needed to check the URI to give the
right answer. This means that part of the application is a big
**if/elsif** procedure. Also, in standard callback it is not possible
to have user data. Both of these restrictions are addressed with the
Dispatchers facilities.

Working with a dispatcher is quite easy:

* Create a new dispatcher by inheriting from the service you want
  to build.
* Register a set of action based on rules (strings, regular
  expressions depending on the service)

.. _Callback_dispatcher:

Callback dispatcher
-------------------

.. index:: Dispatchers callback
.. index:: Callback, dispatcher

This is a wrapper around the standard callback procedure. It is needed
to mix dispatcher based callback and access to procedure
callback. Note that it is not in the `AWS.Services.Dispatchers`
hierarchy but in `AWS.Dispatchers.Callback` because this is a
basic service needed for the server itself. It is referenced here for
documentation purpose but an AWS server can be built with using it.

see :ref:`AWS.Dispatchers.Callback` for complete spec description.

.. _Method_dispatcher:

Method dispatcher
-----------------

.. index:: Dispatchers method
.. index:: method, dispatcher

This is a dispatcher based on the request method. A different callback
procedure can be registered for the supported request methods: GET,
POST, PUT, HEAD.

see :ref:`AWS.Services.Dispatchers.Method` for complete spec description.

.. _URI_dispatcher:

URI dispatcher
--------------

.. index:: Dispatchers URI
.. index:: URI, dispatcher

This is a dispatcher based on the request resource. A different callback
procedure can be registered for specific resources. The resource is
described either by its full name (string) or a regular expression.

see :ref:`AWS.Services.Dispatchers.URI` for complete spec description.

.. _Virtual_host_dispatcher:

Virtual host dispatcher
-----------------------

.. index:: Dispatchers virtual host
.. index:: virtual host, dispatcher

This is a dispatcher based on the host name. A different callback
procedure can be registered for specific host. This is also known as
virtual hosting.

The same computer can be registered into the DNS with different
names. So all names point to the same machine. But in fact you want
each name to be seen as a different Web server. This is called virtual
hosting. This service will just do that, call different **callback**
procedures or redirect to some **machine/port** based on the host name
in the client's request.

see :ref:`AWS.Services.Dispatchers.Virtual_Host` for complete spec description.

.. _Transient_pages_dispatcher:

Transient pages dispatcher
--------------------------

.. index:: Dispatchers Transient pages
.. index:: transient pages, dispatcher

This is a dispatcher that calls a user's callback and if the resource
requested is not found (i.e. the user's callback returns status code
404) it checks if this resource is known as a transient
page. see :ref:`Transient_Pages`.

.. _Timer_dispatcher:

Timer dispatcher
----------------

.. index:: Dispatchers Timer
.. index:: timer, dispatcher

A timer dispatcher can be used to call different callback routines
depending on the current date and time. Such dispatcher is composed of
a set of `Period` activated. When the current date and time is
inside a `Period` the corresponding callback is called. A
`Period` can eventually be repeated. Here are the different kind
of `Period` supported by `AWS`:

*Once*
  A unique period in time. The boundaries are fully described using a
  year, month, day, hour, minute and second.

*Yearly*
  A period that repeats each year. The boundaries are described using a
  month, day, hour, minute and second.

*Monthly*
  A period that repeats each month. The boundaries are described using a
  day, hour, minute and second.

*Weekly*
  A period that repeats each week. The boundaries are described using a
  day name, hour, minute and second.

*Daily*
  A period that repeats each day. The boundaries are described using an
  hour, minute and second.

*Hourly*
  A period that repeats each hour. The boundaries are described using a
  minute and second.

*Minutely*
  A period that repeats each minute. The boundaries are described using
  a second.

.. _Linker_dispatcher:

Linker dispatcher
-----------------

.. index:: Dispatchers Linker
.. index:: linker, dispatcher

A dispatcher that can be used to chain two dispatchers. The response
of the first dispatcher is returned except if it is a 404 (Not Found)
error. In this case, the response of the second dispatcher is returned.

.. _SOAP_dispatcher:

SOAP dispatcher
---------------

.. index:: Dispatchers SOAP
.. index:: SOAP, dispatcher

`AWS` provides also a `SOAP` specific dispatcher. This is a way to
automatically route HTTP requests or `SOAP` requests to different
callback routines.

see :ref:`SOAP_helpers` for more information.
see :ref:`SOAP.Dispatchers.Callback` for complete spec description.

.. _Static_Page_server:

Static Page server
==================

.. index:: Static Page server
.. index:: Simple Page server
.. index:: Page server

This service is a ready to use static page server callback. Using it
is possible to build a simple static page server, as simple as::

 with AWS.Server;
 with AWS.Services.Page_Server;

 procedure WPS is
    WS : AWS.Server.HTTP;
 begin
    AWS.Server.Start
      (WS, "Simple Page Server demo",
       Port           => 8080,
       Callback       => AWS.Services.Page_Server.Callback'Access,
       Max_Connection => 5);

    AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

    AWS.Server.Shutdown (WS);
 end WPS;

Build this program and launch it, it will server `HTML` pages and images
in the current directory.

It is possible to activate the directory browsing facility of this
simple page server. This is not activated by default. This feature
is based on the directory browsing service see :ref:`Directory_browser`.

Note that this service uses two template files:

*aws_directory.thtml*
  The template page used for directory browsing. See
  see :ref:`AWS.Services.Directory` for a full description of this
  template usage.

*404.thtml*
  The Web page returned if the requested page is not found. This is a
  template with a single tag variable named PAGE. It will be replaced by
  the ressource which was not found.

  Note that on Microsoft IE this page will be displayed only if the total
  page size is bigger than 512 bytes or it includes at least one image.

see :ref:`AWS.Services.Page_Server` for a complete spec description.

.. _Transient_Pages:

Transient Pages
===============

.. index:: transient pages
.. index:: pages, transient

A transient page is a resource that has a certain life time on the
server. After this time the resource will be released and will not be
accessible anymore.

Sometimes you want to reference, in a Web page, a resource that is built
in memory by the server. This resource can be requested by the client (by
clicking on the corresponding link) or not, in both cases the page must
be released after a certain amount of time to free the associated memory.

This is exactly what the transient pages high level service do
automatically. Each transient page must be registered into the
service, a specific routine named `Get_URI` can be used to create
a unique `URI` on this server. see :ref:`AWS.Services.Transient_Pages`.

A transient pages dispatcher can be used to build a transient pages
aware server. see :ref:`Transient_pages_dispatcher`.

.. _Split_pages:

Split pages
===========

.. index:: split pages
.. index:: pages, split

It not not very convenient to send back a Web page with a large
table. In such a case it is better to split the table in chunks (20
lines or so) and to send only the first page. This page reference the
next pages and can also contains an index of the pages.

The `AWS`'s split page feature can automatically do that for
you. Given template `Translate_Table` or `Translate_Set` and the
max line per page it returns the first page and create a set of
transient pages for all other pages. A set of template tags are used
to reference the previous and next page and also to build the page index.

There is different ways to split a set of pages and ready-to-use
splitters are available:

*Alpha*
  Split in (at most) 28 pages, one for empty fields, one for all fields
  that start with a digit, and one for each different initial letter.
  see :ref:`AWS.Services.Split_Pages.Alpha`.

*Alpha.Bounded*
  Same as the alpha splitter, but pages larger than a Max_Per_Page value
  are further splitted.
  A secondary index is generated that gives the various pages for a given
  letter. see :ref:`AWS.Services.Split_Pages.Alpha.Bounded`.

*Uniform*
  Split in pages of length Max_Per_Page (except the last one). This
  corresponds to the default service in Split_Pages package.
  see :ref:`AWS.Services.Split_Pages.Uniform`.

*Uniform.Alpha*
  Same as the uniform splitter, but builds in addition an alphabetical
  secondary index from a key field.
  see :ref:`AWS.Services.Split_Pages.Uniform.Alpha`.

*Uniform.Overlapping*
  Same as the uniform splitter, but pages (except the first one)
  repeat Overlap lines from the previous page in addition to the
  Max_Per_Page lines. see :ref:`AWS.Services.Split_Pages.Uniform.Overlapping`.

Using the spliter abstract interface it is possible to build a
customized splitter algorithm. see :ref:`AWS.Services.Split_Pages`.

.. _Download_Manager:

Download Manager
================

.. index:: Download Manager

A server that need to handle lot of large downloads can run out of
connection to answer the standard Web pages. A solution is to increase the
number of simultaneous connections, but this is not really efficient
as a task is created for each connection and does not ensure that all
the connections will be used for the downloads anyway.

The download manager can be used for that, and provides the following
feature:

* use a single task for all downloads
* can be configured to limit the number of simultaneous connections
* downloads past this limit are queued
* send messages to the client with the position in the waiting line
* send messages to the client when the download is about to start

The server must be configured to use dispatchers (standard callbacks
are not supported, note that it is possible to create a dispatcher for
standard callbacks. see :ref:`AWS.Dispatchers.Callback`).

To start the download manager you need to pass the main server
dispatcher object. The start routine will return a new dispatcher,
linked with the download server specific dispatcher, that must be used
to start the standard Web server. See comment in
see :ref:`AWS.Services.Download`.

To queue a download request in the download manager you just need to
create a stream object (can be any kind of stream, see
`AWS.Resources.Streams.*`) for the resource to download.

The download manager needs two templates files:

*aws_download_manager_waiting.thtml*
  This template is used for sending a message to the client when the
  request is on the waiting line. The tags defined in this template file
  are:

  *NAME*
    the name of the resource to download (the filename), this is the
    default filename used for the client side save dialog.

  *RES_URI*
    the URI used to access the resource.

  *POSITION*
    the position in the waiting line (not counting the current served clients).

*aws_download_manager_start.thtml*
  This template is used for sending a message to the client when the
  download is about to start (the request is out of the waiting
  line). The tags defined in this template file are:

  *NAME*
    as above

  *RES_URI*
    as above

.. highlight:: xml

It is important to note that those templates must be reloaded
periodically. The best way to do that in the context of an `HTML`
document is to use a meta-tag. For example to refresh the page every
two seconds::

 <meta http-equiv="refresh" content="2">

The templates could look like:

*aws_download_manager_waiting.thtml*

::

  <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
    <html>
      <head>
        <meta http-equiv="refresh" content="2">
        <title>Download Manager - waiting</title>
      </head>
      <body>
        <p>Waiting for downloading @_NAME_@
        <p>Position in the waiting line @_POSITION_@
      </body>
    </html>

*aws_download_manager_start.thtml*

::

  <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
    <html>
      <head>
        <meta http-equiv="refresh" content="2">
        <title>Download Manager - waiting</title>
      </head>
      <body>
        <p>Waiting for downloading @_NAME_@
        <p>The download will start in a moment
      </body>
    </html>

.. _Web_Elements:

Web Elements
============

.. index:: Web Elements

`AWS` provides some components to help creating nice looking Web
interfaces. It is possible to browse those Web Elements using the
`web_elements` demo. Just launch this Web application from the
demos directory and turn your Web browser to
`http://localhost:2400 <http://localhost:2400>`_.

Currently `AWS` provides:

* Notebooks (based on CSS)
* CSS Menu
* Rounded boxes
* Ajax

All of them are based on templates to be easily reused in other
applications. The three first are best described by the Web Elements
demos as they are 100% design. The `Ajax` one is a bit more complex, we
will present its use in the following section.

.. _Installation:

Installation
------------

.. index:: we_icons
.. index:: we_js

To ease integration we have used the following design:

* Sub-directories found in the `AWS`'s web_elements directory
  are self contained. The content must be copied into the project. Note
  that the icons and javascripts directories contain the
  icons and javascripts code shared by all web elements and must also be
  copied, see below.

* Each graphic elements (icons) is referenced into the templates with the
  alias `/we_icons/<icon_name>`. So users must provide the right alias
  ("`/we_icons/`") in the  Web server.

* Each JavaScripts code is referenced into the templates with the
  alias `/we_js/<script>`. So users must provide the right alias
  (`"/we_js/"`) in the  Web server.

.. _Ajax:

Ajax
----

.. index:: Ajax
.. index:: aws_action_replace.tjs
.. index:: aws_action_clear.tjs

First of all, `Ajax` stand for
*Asynchronous JavaScript language and XML*, and is not well defined
at the moment. `Ajax` is on one side able to send HTTP requests
to the Web server and on the other side able to manipulate directly the Web
browser's `DOM` tree. On the `DOM` it can add, remove or
replace `XML` nodes. So, it is possible to change the content of
a Web page without reloading it from the server.

Most importantly, `Ajax` changes the way Web applications are
thought from **page** based to **event** based.

As implemented into `AWS`, `Ajax` support comes as a set of
`JavaScript` templates. Using those templates there is no need to
know `JavaScript` (except for the `JavaScript` event names) and it
makes `Ajax` programming lot easier. Two actions are provided,
one for replacing another for clearing part of the web page content.

.. _Steps_to_do_Ajax:

Steps to do Ajax
^^^^^^^^^^^^^^^^

What are the steps to do `Ajax` ?

Remember, do not think about the Web page but about a specific widget
(`HTML` fragments) with the associated event and action.

* Include the AWS/Ajax support file

  This is the `AWS/Ajax` runtime, it contains `JavaScript`
  code needed for the `AWS/Ajax` support.

* Create the Web widgets/forms

  There is nothing special here, use your favorite Web designer tool.

* Create Web area

  Using some `HTML` <div> tags we create areas where we will place
  `HTML` fragments later. For example when clicking on a button
  (described above) in our Web interface we want to display a new form
  in this area.

* Name the widgets/forms/area using id="name" attribute

  Give a different name to the widgets using id="name". This name will
  be later used to identify the widgets on which the envent and
  corresponding action must be placed. We do not want to clutter the Web
  design with `JavaScript` code like `onclick="dothis()"` or
  `onchange="dothat()"`.

* Add the proper event/action to the widgets using the AWS/Ajax templates

  This is the interresting part. At this point we link events/actions
  to the widgets and specify in which area the results sent by the
  server will be placed.

This is not the only way to do `Ajax`, we just presented here a simple
approach that works well with the `AWS/Ajax` templates.

.. _Basic_Ajax_support:

Basic Ajax support
^^^^^^^^^^^^^^^^^^

This section describes the `AWS/Ajax` support where the answer from the
server is an `HTML` fragment. This basic support is designed to
be used for migration of a Web server to `Ajax`. For new
applications, it is worth considering using the XML based Ajax support,
see :ref:`XML_based_Ajax`.

Let's have a very simple example:

* The AWS/Ajax runtime support

  ::

   @@INCLUDE@@@ aws.tjs

  Must be included in every Web pages into the `<head>` tag.

* The widget: a button

  ::

   <input id="clickme" type="button" value="Clik Me">

* The result area: a div

  ::

   <div id="placeholder">... result here ...</div>

* The AWS/Ajax

  ::

   @@INCLUDE@@ aws_action_replace.tjs onclick clickme placeholder

  Basically it places an **onclick** attribute (the event) in the `HTML`
  `<input>` identified as **clickme** (the action) above. Here is
  what happen when the button is clicked:

  * send the "/onclick$clickme" HTTP request to the server
  * asynchronously wait for the answer, when received place the
    message body into the `<div>` **placeholder**.

.. highlight:: ada

On the server side the code would look like this::

 function Callback (Request : in Status.Data) return Response.Data is
    URI : constant String := Status.URI (Request);
 begin
    if URI = "/clickme" then
       return Response.Build (MIME.Text_HTML, "you click me!");
    ...

So when the button is clicked the string **"you click me!"** will replace
the **"... result here ..."** string of the place holder div above.

This is a simple and very limited example as there is no parameter
passed to the `HTTP` request. In real Web applications it is necessary
to send a context with the request. This can be either the value of
other widgets or all values of widgets' form.

.. highlight:: xml

References to widgets or forms can be passed to the
:file:`aws_action_replace.tjs` template starting with the 5th
parameter::

 <input id="field" type="text" value="default value">

 ...

 @@INCLUDE@@ aws_action_replace.tjs (onclick clickme placeholder 5=>field)

or::

 <form id="small_form" name="small_form">
 ...
 </form>

 @@INCLUDE@@ aws_action_replace.tjs (onclick clickme placeholder 5=>*mall_form)

Note that the `onclick` event is only one of the possible
`JavaScript` event on a `button`. It is possible to used
any supported event, for example on an `HTML` `<select>` widget
it is common to map the action to the `onchange` event.

`AWS` also provides support for clearing an area or a widget
content (like an input)::

 @@INCLUDE@@ aws_action_clear.tjs (onclick, clear, field)

This simple action adds the **onclick** event to the **clear** button
to erase the content of the **field** widget.

.. _XML_based_Ajax:

XML based Ajax
^^^^^^^^^^^^^^

In many cases you'll like to update and/or clear multiple areas in your
Web interface. With the templates above only a single action is
possible. `AWS` provides support for `XML` based answers. In
this `XML` documents it is possible to:

* replace an area with a new content::

   <replace id="item_id">new text</replace>

* clear an area::

   <clear id="item_id"/>

* add an item into a select widget::

   <select action="add" id="item_id"
           option_value="value" option_content="content"/>

* remove an item from a select widget::

   <select action="delete" id="item_id" option_value="value"/>

* select a specific item in a select widget::

   <select action="select" id="item_id" option_value="value"/>

* clear a select widget (remove all items)::

   <select action="clear" id="item_id"/>

* select a radio button::

   <radio action="select" id="item_id"/>

* check a checkbox::

   <check action="select" id="item_id"/>

* clear a checkbox::

   <check action="clear" id="item_id"/>

* call another URL::

   <get url="http://thishost/action">
     <parameters value="name=Ajax"/>
     <field id="input1"/>
   </get>

  This will send the following request::

   http://thishost/action?name=Ajax&input1=<val_input1>

  Where **val_input1** is the current value of the **input1** input
  widget. The result must be an `XML/Ajax` document that will be parsed.

* make a list sortable::

   <make_sortable>
     <list id="firstlist"/>
     <list id="secondlist"/>
   </make_sortable>

  Here **firstlist** and **secondlist** are **id** of `UL` elements. It is
  possible to specified as many list id as needed. A drag and drop is
  then possible for all elements in those lists. It is then possible to
  reference such list by passing the list id as a field to the
  template. Items on those list will be serialized and passed to the `AWS`
  callback. Note that for the serialization to work properly, each
  `LI` elements must be given the id of the list and then the value
  we want to pass::

   <ul id="firstlist">
     <li id="firstlist_red">Red</li>
     <li id="firstlist_green">Green</li>
     <li id="firstlist_blue">Blue</li>
   </ul>

  The serialization will send each value on this list using a
  multi-valued parameter named **firstlist[]**::

   http://server?firstlist[]=red&firstlist[]=green&firstlist[]=blue

* make a list not sortable::

   <destroy_sortable>
     <list id="firstlist"/>
     <list id="secondlist"/>
   </destroy_sortable>

  Remove the sortable properly from the specified lists.

* redirect to another URL::

   <location url="http://thishost/go_there"/>

  Redirect the browser to the specified URL.

* refresh the current page::

   <refresh/>

  Refresh the current page as if the Web Browser refresh button was pressed.

* add a CSS style to a given node::

   <apply_style id="node_id">
     <attribute id="display" value="none"/>
   </apply_style>

  Add the CSS style `display:none` to the **node_id** element. It
  is possible to specify multiple attributes if needed.

* make an entry disabled or enabled::

   <disabled id="item_id" value="true/false"/>

* make an entry read-only or writable::

   <read_only id="item_id" value="true/false"/>

* reset a form::

   <reset id="form_id"/>

Here is an example of such XML document::

 <?xml version="1.0" encoding="UTF-8" ?>
 <response>
   <replace id="xml_status_bar">Fill Widgets...</replace>
   <replace id="text1">Response from XML</replace>
   <replace id="text2">Another response for text2</replace>
   <replace id="input1">tag is input1</replace>
   <replace id="input2">tag is input2</replace>
   <select action="add" id="xmlsel" option_value="one" option_content="1"/>
   <select action="add" id="xmlsel" option_value="two" option_content="2"/>
   <select action="add" id="xmlsel" option_value="three" option_content="3"/>
   <select action="select" id="xmlsel" option_value="two"/>
   <radio action="select" id="radio1"/>
   <check action="select" id="check1"/>
   <check action="select" id="check3"/>
   <check action="clear" id="check2"/>
 </response>

To register an `Ajax` action to a specific tag id a macro can be
used. It is named `JS_ACTION` and defined in :file:`ajax_api.tjs`.
The usage is similar to what is described in the previous section
(see :ref:`Basic_Ajax_support`) except that in this case we use a macron
instead of an include file and we do not have to pass the placeholder.

Let's revisit the first example above to use the `XML`
`Ajax` support.

* The AWS/Ajax runtime support::

   @@INCLUDE@@@ aws.tjs

  Must be included in every Web pages into the `<head>` tag.

* The AWS/Ajax API::

   @@INCLUDE@@@ ajax_api.tjs

  Must be included at least once during an application life-time. It
  gives access to the `JS_ACTION` macro.

* The widget: a button::

   <input id="clickme" type="button" value="Clik Me">

* The result area: a div::

   <div id="placeholder">... result here ...</div>

* The AWS/Ajax::

   @_JS_ACTION(onclick, clickme)_@

  Basically it places an **onclick** attribute (the event) in the `HTML`
  `<input>` identified as **clickme** (the action) above. Here is
  what happen when the button is clicked:

  * send the "/onclick$clickme" HTTP request to the server
  * asynchronously wait for the XML answer, when received parse the
    answer and perform the actions according to the `XML` content.

To set the placeholder with "**new text**", the `XML` document
returned by the server must be::

 <?xml version="1.0" encoding="UTF-8" ?>
 <response>
   <replace id="placeholder">new text</replace>
 </response>

If we want also to clear the input field named **field** and to select the
radio button named **radio1** we must return::

 <?xml version="1.0" encoding="UTF-8" ?>
 <response>
   <replace id="placeholder">new text</replace>
   <clear id="field"/>
   <radio action="select" id="radio1"/>
 </response>

This is by far the most flexible solution as it is possible to return, from the
server, a structured answer.

A final comment, if the text returned by the server to replace a
specific area is an `HTML` fragment, the content must be placed into a
`CDATA` tag::

 <?xml version="1.0" encoding="UTF-8" ?>
 <response>
   <replace id="item_id">
     <![CDATA[ *HTML CODE HERE* ]]>
   </replace>
 </response>

.. _Advanced_Ajax:

Advanced Ajax
^^^^^^^^^^^^^

Finally, if this is not enough because you need to use some specific
`JavaScript` code, `AWS` provides a macro named
`BIND_JS` to add an event to a specific widget, the action being
the name of a `JavaScript` routine.

This macro together with the :file:`aws_func_replace.tjs`,
:file:`aws_func_clear.tjs` templates and the :file:`JS_ACTION` macro can
be used to chain multiple actions. Those templates are the function
body used by the corresponding templates :file:`aws_action_replace.tjs`,
:file:`aws_action_clear.tjs`.

Let say you want to clear a widget, change the content of another one
and calling one of your specific `JavaScript` routine when clicking on
a button. It is not possible to have mutiple `onclick` events on
the same widget, the solution is the following:

* Create the JavaScript routine to do the job

  For this in the the body of the `clear_replace()` JavaScript
  routine we place::

   function clear_replace()
   {
     @@INCLUDE@@ aws_func_replace.tjs (clickme placeholder 4=>field)
     @@INCLUDE@@ aws_func_clear.tjs (area)
     call_this_routine();
   }

  Then to add the event on the widget::

   @_BIND_JS(onclick, clickme clear_replace)_@

Furthermore, it is possible to pass (as the parameter number 20) a
routine to call after a specific action to all templates and to the
`JS_ACTION` macro. This is another way to chain multiple actions
for a single event.

Note that all `AWS/Ajax` templates and the :file:`ajax_api.tjs`
file have a set of comments at the start explaining in details the
usage of each parameter.

.. _Web_Blocks:

Web Blocks
==========

.. index:: Web Blocks

The `AWS.Services.Web_Block` hierarchy contains an API useful for
keeping context on Web pages. It has been designed to be able to split
a Web application into a set of independent blocks that can be put
together in the same Web page. The context is then useful as it is
passed and known by each individual block. Note that this is different
than the session as a session is global to the current Web browser
whereas the context can be different for each individual web pages
opened.

Instead of parsing a whole page using `AWS.Templates` API the web blocks
are registered independently using `AWS.Services.Web_Block.Registry`.
The block is registered together with its templates and a callback to use
to get user's data for this specific block with the given context.

So using this API, instead of having a set of callbacks returning an
`AWS.Response.Data` and where the final rendering is to be done
by the client code, we have a set of callbacks that returns a
`Translate_Set`. The client just have to fill the set with the
data corresponding to the actual request and possibly using the
context. The final rendering is done by the provided services in
`Web_Block.Registry`.

Note that all Web pages must also be registered into the registry
to ensure that the context identification is properly kept. The context
identification is injected into the Web pages transparently for the
end-user when using `Ajax`.

.. _Web_Block_example:

Web Block example
-----------------

Let's have a simple example, a page containing a single block with a
tag (@_COUNTER_@) which is incremented by one each time it is
used. The code can be found in :file:`demos/web_block`.

First create the following HTML fragment and place it into
:file:`counter.thtml`::

 <p>@_COUNTER_@</p>

Then create the main page and place it into :file:`page.thtml`. The
important part is the @_CTX_WB_@ tag which is passed to the link. This
tag is the context identifier, it must be passed to each request. Note
that this is automatically done when using the `Ajax` framework
(see :ref:`Web_Block_and_Ajax`)::

 <html>
   <head>
     <title>Main Page</title>
   </head>
   <body>
     <p>This is the main page, bellow is a simple counter</p>
     <p>@_COUNTER_@</p>
     <a href="/?CTX_WB=@_CTX_WB_@>Next</a>
   </body>
 </html>

.. highlight:: ada

The `Web_Callbacks` package contains the application callbacks::

 with AWS.Response;
 with AWS.Status;
 with AWS.Templates;
 with AWS.Services.Web_Block.Context;

 package Web_Callbacks is

    use AWS;
    use AWS.Services;

    function Main (Request : in Status.Data) return Response.Data;
    --  Main callback which handle the home page

    procedure Counter
      (Request      : in              Status.Data;
       Context      : not null access Web_Block.Context.Object;
       Translations : in out          Templates.Translate_Set);
    --  The callback handling the counter web block

 end Web_Callbacks;

Last part is to actually implement the `Counter` callback. Here
is a possible implementation making use of the context to keep the
counter state::

 with AWS.Utils;
 with AWS.Messages;
 with AWS.MIME;
 with AWS.Services.Web_Block.Registry;

 package body Web_Callbacks is

    -------------
    -- Counter --
    -------------

    procedure Counter
      (Request      : in              Status.Data;
       Context      : not null access Web_Block.Context.Object;
       Translations : in out          Templates.Translate_Set)
    is
       N : Natural := 0;
    begin
       if Context.Exist ("N") then
          N := Natural'Value (Context.Get_Value ("N"));
       end if;

       N := N + 1;
       Context.Set_Value ("N", Utils.Image (N));

       Templates.Insert
         (Translations, AWS.Templates.Assoc ("COUNTER", N));
    end Counter;

    ----------
    -- Main --
    ----------

    function Main (Request : in Status.Data) return Response.Data is
       URI : constant String := Status.URI (Request);
    begin
       return Web_Block.Registry.Build
         (Key          => URI,
          Request      => Request,
          Translations => Set);
    end Main;

 end Web_Callbacks;

Finally, we write the main procedure::

 with Ada.Text_IO;

 with AWS.Server;
 with AWS.Services.Web_Block.Registry;

 with Web_Callbacks;

 procedure Web_Block is

    use Ada;
    use AWS;
    use AWS.Services;

    HTTP : AWS.Server.HTTP;

 begin
    --  First we register the main page and the counter block

    Services.Web_Block.Registry.Register ("/", "page.thtml", null);

    Services.Web_Block.Registry.Register
      ("COUNTER", "counter.thtml",
       Web_Callbacks.Counter'Access, Context_Required => True);

    --  Then we just start the server

    Server.Start (HTTP, "web_block", Web_Callbacks.Main'Access);

    Text_IO.Put_Line ("Press Q to terminate.");

    Server.Wait (Server.Q_Key_Pressed);

    Server.Shutdown (HTTP);
 end Web_Block;

Compile and run the server. Then connect to the server and click on
next. The counter will be incremented by one each time.

.. _Web_Block_and_Ajax:

Web Block and Ajax
------------------

The Web Block framework has really been designed to be used with
`Ajax`. It is the only way to gain the full power of the Web Block
framework.

For the complete code, see `demos/web_block_ajax`.

.. highlight:: xml

When using `Ajax` it is not needed to explicitly pass the context
identification to every link. This is done automatically by the
framework. So the main page will look like this::

 @@INCLUDE@@ ../../web_elements/javascripts/ajax_api.tjs
 <html>
   <head>
     <title>Main Page</title>
     @@INCLUDE@@ ../../web_elements/javascripts/aws.tjs
   </head>
   <body>
     <p>This is the main page, bellow is a simple counter</p>
     @_WIDGET_COUNTER_@
   </body>
 </html>

The counter widget is on :file:`widget_counter.thtml`::

 <!-- implementation of a simple counter widget -->
 <p><div id="counter">@_COUNTER_@</div></p>
 <a id="next" href="/">Next</a>
 @_JS_ACTION(onclick, next)_@

For the `Ajax` part, see :ref:`Ajax`.

.. highlight:: ada

We now have one more register call for registering the `next` button
`Ajax` callback, and a callback named `Widget_Counter` for
displaying the block::

 Services.Web_Block.Registry.Register
   ("WIDGET_COUNTER", "widget_counter.thtml",
    Web_Callbacks.Widget_Counter'Access);

 Services.Web_Block.Registry.Register
   ("/onclick$next", "r_widget_counter.txml",
    Web_Callbacks.Onclick_Next'Access,
    Content_Type     => MIME.Text_XML,
    Context_Required => True);

.. highlight:: xml

The `next` `Ajax` button is using an XML based response which
is defined in :file:`r_widget_counter.txml`::

 <?xml version="1.0" encoding="UTF-8" ?>
 <response>
   <replace id="counter">@_COUNTER_@</replace>
 </response>

.. highlight:: ada

The `Widget_Counter` callbacks just have to set the
`COUNTER` tag variable to the corresponding value. This is used to
display the block. The `Ajax` callback `Onclick_Next` has to
increment the counter and set the `COUNTER` tag variable, a simple
implementation is::

 procedure Onclick_Next
   (Request      : in              Status.Data;
    Context      : not null access Web_Block.Context.Object;
    Translations : in out          Templates.Translate_Set)
 is
    N : Natural := 0;
 begin
    if Context.Exist ("N") then
       N := Natural'Value (Context.Get_Value ("N"));
    end if;

    N := N + 1;

    Context.Set_Value ("N", Utils.Image (N));

    Templates.Insert
      (Translations, Templates.Assoc ("COUNTER", N));
 end Onclick_Next;

The framework will then call `Onclick_Next` when pressing the
`Next` button. This routine increments N by one sending back a
response based on `r_widget_counter.txml`. Finally, the client
browser will parse this XML response and do the corresponding actions.

.. _Web_Block_and_templates2ada:

Web Block and templates2ada
---------------------------

For the complete code, see `demos/web_block_ajax_templates`.

It is possible to use the `Templates_Parser's templates2ada` tool for
generating the callbacks register calls. This ensures that all tags on the
application Web Pages have a corresponding callback.

.. highlight:: xml

The code is almost identical to the standard `Ajax` example above. The
main difference is that we need to use a naming convention for the
blocks. This way we can generate automatically the corresponding
callbacks using a template. A common convention is to add `LAZY_` as
prefix for the name of the blocks. With this convention the main page
template is::

 @@INCLUDE@@ ../../web_elements/javascripts/ajax_api.tjs
 <html>
   <head>
     <title>Main Page</title>
     @@INCLUDE@@ ../../web_elements/javascripts/aws.tjs
   </head>
   <body>
     <p>This is the main page, bellow is a simple counter</p>
     @_LAZY_WIDGET_COUNTER_@
   </body>
 </html>

.. highlight:: ada

We need also modify the standard :file:`templates.tads` as distributed
with the `Templates_Parser`. Here is the interesting part::

 @@SET@@ PACKAGE = WBlocks

 ...

 with AWS.MIME;
 with AWS.Services.Web_Block.Registry;
 with Web_Callbacks;

 @@TABLE@@
 with @_PACKAGE_@.@_CAPITALIZE:REPLACE_ALL(\\./_):BASENAME_@;
 @@END_TABLE@@

 package body @_PACKAGE_@ is

    use AWS;

    package body Lazy is

       --------------
       -- Register --
       --------------

       procedure Register is
          use AWS.Services;
       begin
          --  Register blocks
          @@TABLE@@
          @@IF@@ @_UPPER:SLICE(1..5):VARIABLE_LIST_@ = "LAZY_"
          Web_Block.Registry.Register
            ("@_VARIABLE_LIST_@",
             "@_LOWER:REPLACE_ALL(LAZY_/):VARIABLE_LIST_@.thtml",
             Web_Callbacks.@_CAPITALIZE:REPLACE_ALL(LAZY_/):VARIABLE_LIST_@'Access);
          @@END_IF@@
          @@END_TABLE@@

          --  Register Ajax
          @@TABLE@@
          @@TABLE@@
            @@IF@@ not @_IS_EMPTY:AJAX_EVENT_@
          Services.Web_Block.Registry.Register
            ("/@_AJAX_EVENT_@$@_AJAX_ACTION_@",
             @_PACKAGE_@.R_@_CAPITALIZE:REPLACE_ALL(\\./_):AJAX_FILE_@.Template,
             Web_Callbacks.@_CAPITALIZE:AJAX_EVENT_@@_UNDERSCORE_@@_CAPITALIZE:AJAX_ACTION_@'Access,
             Content_Type     => MIME.Text_XML,
             Context_Required => True);
            @@END_IF@@
          @@END_TABLE@@
          @@END_TABLE@@
       end Register;
    end Lazy;
 end @_PACKAGE_@;

Basically this is to write a register call for every template's
tag starting with `LAZY_`. The second section is to write a
register call for every `Ajax` event. All callbacks are expected to be in
a package named `Web_Callbacks`. It is of course possible to change
this template to reference callbacks for blocks and `Ajax` in separate
packages. The use of a template here is very flexible.

Now let's parse the application HTML and XML templates and create the
corresponding Ada specs and register calls::

 $ templates2ada -d . -o code.ada -t templates.tada -e .thtml -e .txml
 $ gnatchop code.ada

Look at the generated code below, it properly register the
`Widget_Counter` callback to be used for rendering
`LAZY_WIDGET_COUNTER` using the :file:`widget_counter.thtml`. So
we have a tight coupling between the code and the template file. If
the tag is renamed in the template file the application will not
compile anymore. The same is true for `Ajax` callbacks, every
`Ajax` action put in a template file needs a corresponding
callback in Ada. This greatly helps keeping the application code
synchronized::

 procedure Register is
    use AWS.Services;
 begin
    Web_Block.Registry.Register
      ("LAZY_WIDGET_COUNTER",
       "widget_counter.thtml",
       Web_Callbacks.Widget_Counter'Access);
    Services.Web_Block.Registry.Register
      ("/onclick$next",
       WBlocks.R_Widget_Counter.Template,
       Web_Callbacks.Onclick_Next'Access,
       Content_Type     => MIME.Text_XML,
       Context_Required => True);
 end Register;

In the main, it is just now required to register the Web pages and to
call the generated `Register` procedure::

 Services.Web_Block.Registry.Register ("/", "page.thtml", null);

 WBlocks.Lazy.Register;

Moreover, an Ada spec containing reference for the tag names is
generated for every HTML and XML template file. All tags can be
referenced using those specs, it is not needed to use string
literal in the application. Again, this ensures that a tag which is
renamed or deleted is detected at compilation time. For example the
`Widget_Counter` callback can be rewritten as follow::

 procedure Widget_Counter
   (Request      : in              Status.Data;
    Context      : not null access Web_Block.Context.Object;
    Translations : in out          Templates.Translate_Set)
 is
    N : Natural := 0;
 begin
    if Context.Exist ("N") then
       N := Natural'Value (Context.Get_Value ("N"));
    end if;

    Templates.Insert
      (Translations, Templates.Assoc (WBlocks.Widget_Counter.COUNTER, N));
 end Widget_Counter;

.. _Web_Cross-References:

Web Cross-References
====================

.. index:: webxref
.. index:: web cross-references

When building an `Ajax` Web applications it is required to give ids to
web elements to be able to reference them. It is also quite common to
use CSS to give such and such item a specific style. After some time
it is quite difficult to keep track of all those ids. Are they all
used ? Don't we reference an id that does not exist anymore ?

`webxref` has been designed to help finding such problems.

The files kinds handled are:

*.css*, *.tcss*
  A CSS (or template CSS file). Ids and classes inside are recorded as
  CSS definitions.

*.xml*, *.html*, *.thtml*
  A meta-language document. Ids and classes inside are recorded as
  referencing a CSS definition and meta-language definition.

*.txml*
  An `Ajax` response file. Ids declared inside are recorded as referencing
  a meta-language definition.

The features are:

*cross-references*
  By default `webxref` output all the references to ids and classes.

*finding unused items*
  Output the ids/classes that are defined but not used. For example an
  id declared in a CSS but never referenced into an HTML document or an
  HTML id never referenced in an `Ajax` response file :file:`.txml` document.

*finding undeclared items*
  Output ids/classes that are referenced but never defined. This is for
  example an id inside an `Ajax` response file which is never defined into
  an HTML document.

*enforcing a naming scheme for ids and classes*
  It can enforce a specific prefix for ids and classes. The id prefix
  can be based on the filename (using filename's first character and all
  character before an underscore). This make it less likely to find the
  same id on multiple files.

Note that all references are in a format recognized by tools like `GPS`
and `Emacs`. It is then possible to navigate inside them easily.

All `webxref` options are listed using the `-h` option.

.. _WebSockets:

WebSockets
==========

.. index:: websockets
.. index:: web sockets

.. _Introduction_to_WebSockets:

Introduction to WebSockets
--------------------------

WebSockets are part of HTML5, the API is being standardized by the W3C
and the protocol by the IETF (see RFC-6455). It is a bidirectional and
full-duplex communication channel between the client and the
server. Most Web Browsers are now supporting (at least part) of the
WebSocket recommendation. On the client side, the WebSockets are
programmed in JavaScript as done for Ajax for example.

A WebSocket is always opened at the request of a client. This can be
done on the same port as the main HTTP protocol. This is possible because
the initial handshake to open a WebSocket is done in pure HTTP protocol. Past
this initial handshake the socket is switching protocol from HTTP to the one
called WebSocket protocol.

It is not needed to know the protocol to use the WebSockets, AWS comes with
some high level services on the server side and also on the client side.

.. _WebSockets_on_the_client:

WebSockets on the client (javascript)
-------------------------------------

The WebSocket is created on the client side. As there is some differences
between Web browsers, AWS provides a wrapper routine to create a
WebSocket::

 ws = AWS.WebSocket.open('ws://localhost:8080/echo');

This basically create a WebSocket and contact the local server using
port 8080.

This method is declared into :file:`aws.tjs` which must be included::

 @@INCLUDE@@@ aws.tjs

A WebSocket Javascript's object has four method's callbacks:

*onopen*
  Called when the WebSocket has been opened. This means that the
  initial handshake with the server has been accepted. At this point the
  WebSocket is ready to send and received messages.

*onmessage*
  Called for every incoming message. This callback receive a single
  parameter which is the event. The actual message data can be found in
  **e.data**.

*onclose*
  Called when the WebSocket is closing. This means that the server has
  sent a close request. After this event it is not possible to send nor
  receive messages through this WebSocket.

*onerror*
  Called when an error has occurred. This can be a lost connection for
  example. This callback takes a single parameter which is the error
  message.

AWS comes with default implementation of those callbacks. With the
two optional WebSocket constructor parameters it can be configured to
fit most needs::

 ws = AWS.WebSocket.open('ws://localhost:8080/echo', message_id, status_id);

*message_id*
  The id of the HTML element which will be used to display the incoming
  messages. This is most of the time the id of a `p` or `div` HTML
  element.

*status_id*
  The id of the HTML element which will be used to display the status
  and error messages. For example when a connection is closed.

When those default callbacks are not what is needed it is always
possible to redefine them::

 ws.onmessage = function (e) {
   code there
 };

Likewise for the other events.

.. _WebSockets_on_the_server:

WebSockets on the client (Ada)
------------------------------

AWS also supports writing websocket clients directly in Ada. Here is an
example::

   type MySocket is new AWS.Net.WebSocket.Object with null record;
   overriding procedure On_Message (Self : in out MySocket; Str : String);
   --  You would likely also override On_Error and On_Close

   overriding procedure On_Message (Self : in out MySocket; Str : String) is
   begin
      Ada.Text_IO.Put_Line ("++ Got message '" & Str & "'");
   end On_Message;

   declare
      Socket     : MySocket;
   begin
      AWS.Net.WebSocket.Connect (Socket, "ws://localhost:8765");

      --  Send one message
      Socket.Send ("some message");

      --  Then wait for any number of messages from the server. Give up if
      --  no message is available for 2s. If messages become available, the
      --  procedure On_Message will be called.
      while Socket.Poll (Timeout => 2.0) loop
         null;
      end loop;

      Socket.Close ("");
   end;

You are responsible for checking regularly whether any message has been
received from the server.

WebSockets on the server
------------------------

The first step is to setup the server to dispatch the incoming
messages to the proper WebSocket object. For this one needs to inherit
from `AWS.Net.WebSocket.Object` and redefine at least two methods
`Create` and `On_Message`:

*Create*
  This is the constructor that will be used by the server to handle some
  WebSockets. This constructor will be associated to some URI, see below::

   function Create
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return Object'Class;

  The default constructor creates a WebSocket of type
  `AWS.Net.WebSocket.Object`. It is not possible to receive events
  (close, open, error) using such object it is only possible to
  send messages to the clients.

  Here is an example on a custom socket::

   type MySocket is new Net.WebSocket.Object with null record;

   function Create
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return AWS.Net.WebSocket.Object'Class
   is
      --  Note the call to the other version of Create*
      return MySocket'
        (AWS.Net.WebSocket.Object
          (AWS.Net.WebSocket.Create (Socket, Request)) with null record);
   end Create;

  It is also possible to deny the handshake by returning an object from
  AWS.Net.WebSocket.Handshake_Error.

*On_Open*
  This is the callback that will be called when the WebSocket is opened::

   procedure On_Open
     (Socket : in out Object; Message : String) is null;

*On_Message*
  This is the callback that will be called for every message sent by the
  client on the corresponding WebSocket::

   procedure On_Message
     (Socket : in out Object; Message : String);

  The first parameter is the WebSocket itself, it is possible to send a
  message directly by using the associated `Send` method. Note that
  the default implementation supports the XML based Ajax actions.
  See see :ref:`XML_based_Ajax` and can be used to redirect simple message
  to an HTML widget given it's id.

*On_Close*
  This is the callback that will be called when the WebSocket is closed::

   procedure On_Close
     (Socket : in out Object; Message : String) is null;

*On_Error*
  This is the callback that will be called when an error occurs on the
  WebSocket::

   procedure On_Error
     (Socket : in out Object; Message : String) is null;

When this is done, the constructor declared above needs to be
registered to handle some WebSocket designated by the URI. For example
to have this WebSocket handling all URI named `/echo`::

 Net.WebSocket.Registry.Register ("/echo", CB.Create'Access);

Where `CB.Create` is the constructor redefined for the new
WebSocket class.

The last step is to start the WebSocket server which are needed to
handle the incoming messages::

 Net.WebSocket.Registry.Control.Start;

At this point all is setup to have AWS supports WebSockets. Sending
messages can be done to a single client or by broadcasting to all
clients for a specific URI. To send a message one need to create a
`Net.WebSocket.Registry.Recipient` object. For example to
broadcast a message to all Web clients having opened the `/echo`
WebSocket::

 Rcp : Net.WebSocket.Registry.Recipient :=
         Net.WebSocket.Registry.Create (URI => "/echo");

 Net.WebSocket.Registry.Send (Rcp, "A simple message");

As we have seen before, this will send a message to clients which will
in turn trigger the `onmessage` Javascript method.

It is also possible to send a message to clients from a specific
origin by using the `Origin` information::

 Rcp : Net.WebSocket.Registry.Recipient :=
         Net.WebSocket.Registry.Create (URI => "/echo"; Origin => ".*\\.fr");

 Net.WebSocket.Registry.Send (Rcp, "A simple message");

The above recipent targets all WebSockets whose URI is `"/echo"`
and that have been created from a Web page originating from a Web server
running in the `.fr` domain. Note that `URI` and the
`Origin` are regular expressions.

The `Origin` value can be used by a server to handle only
WebSockets originating from it's own domain. Restricting the origin of
the WebSockets can be done with the `WEBSOCKET_ORIGIN` config
parameter, see :ref:`WebSocket_Origin`.
