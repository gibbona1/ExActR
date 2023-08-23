function colourExtentDiag(el, colour) {
  
  // Get all rows of the tbody
  const table = document.getElementById(el);
  const tbody = table.querySelector('tbody')
  const rows = tbody.getElementsByTagName('tr');

  // Loop through rows
  for (let i = 0; i < rows.length; i++) {
    // Get cells of current row
    const cells = rows[i].getElementsByTagName('td');
    
    // Apply color to diagonal cell
    cells[i+1].style.backgroundColor = colour;
  }

  // Find all elements with class "notes-wel"
  const notesElements = document.querySelectorAll('.notes-well');

  // Loop through each element
  notesElements.forEach(element => {
    const originalText = element.innerHTML;
    const modifiedText = originalText.replace(/(<span\b[^>]*>.*?)?diagonal(<\/span>)?/g, '<span style="background-color: '+colour+';">diagonal</span>');
    element.innerHTML = modifiedText;
  });
}