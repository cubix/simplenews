function escapeVal(strIn)
{
  var replaceWith = '<br/>';
  var strTmp = escape(strIn);
  for(i = 0; i < strTmp.length; i++) 
    {
      if (strTmp.indexOf("%0D%0A") > -1)
	{
	  strTmp = strTmp.replace("%0D%0A",replaceWith);
	}
      else if (strTmp.indexOf("%0A") > -1)
	{
	  strTmp = strTmp.replace("%0A",replaceWith);
	}
      else if (strTmp.indexOf("%0D") > -1)
	{
	  strTmp = strTmp.replace("%0D",replaceWith);
	}
    }
  strTmp = unescape(strTmp);
  return strTmp;
}
