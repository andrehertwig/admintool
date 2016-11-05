var allFilesSelected = false;
var allDirsSelected = false;
$( document ).ready(function() {
	$('#selectFiles').on('click', function() {
		$('input.file').each(function() {
			$(this).prop( "checked", !allFilesSelected )
		});
		allFilesSelected = !allFilesSelected;
	});
	
	$('#selectDirs').on('click', function() {
		$('input.dir').each(function() {
			$(this).prop( "checked", !allDirsSelected )
		});
		allDirsSelected = !allDirsSelected;
	});
});