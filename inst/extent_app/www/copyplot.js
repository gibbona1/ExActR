async function getImageBlobFromUrl(url) {
  const fetchedImageData = await fetch(url);
  const blob = await fetchedImageData.blob();
  return blob;
}

async function copyplot(plot_id){
  const src = $("#" + plot_id + ">img").attr("src");
  try {
    const blob = await getImageBlobFromUrl(src);
    await navigator.clipboard.write([
      new ClipboardItem({
        [blob.type]: blob
      })
    ]);
    alert("Image copied to clipboard!");
  } catch (err) {
    console.error(err.name, err.message);
    alert("There was an error while copying image to clipboard :/");
  }
}

function get_img_src(id){
    var src = document.getElementById(id).childNodes[0].src;
    Shiny.setInputValue(id+"_img_src", src);
}