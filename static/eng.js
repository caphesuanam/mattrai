
var timeoutPeriod = 10000;
function setReload()
{
  $.ajax({
          type: "HEAD",
          async: true,
          url: location
      }).fail(function (xhr, textStatus, errorThrown){
        setTimeout(setReload, 10000);
      }).success(function(xhr, textStatus, errorThrown) {
        location.reload();
      });
}
setTimeout(setReload, 10000);
