//  From http://www.quirksmode.org/js/cookies.html


// Create a new cookie for the sever, valid for the given number of days.
// If days is null, then the cookie will expire when the browser is closed
// If days is -1, the cookie is erased
//
// Path is the URL in the domain for which the cookie should be sent. By
// default this is '/'.

function createCookie(name,value,days,path)
{
	if (days)
	{
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";

        if (path)
           var fullpath=path;
        else
           var fullpath="/";

	document.cookie = name+"="+value+expires+"; path="+fullpath;
}

// Read the value for a given cookie
// null is returned if there is no such cookie

function readCookie(name)
{
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++)
	{
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

// Erase a given cookie

function eraseCookie(name)
{
	createCookie(name,"",-1);
}
