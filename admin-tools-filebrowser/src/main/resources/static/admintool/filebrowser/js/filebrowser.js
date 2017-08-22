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
	
	$('#createDir').on('click', function() {
		
		getByID("createFolderModal").modal();
	});
	
	$('#uploadFile').on('click', function() {
		
		getByID("uploadModal").modal();
	});
	
	$('.delete').each(function() {
		var btn = $(this);
		btn.on('click', function() {
			var clickedBtn = $(this);
			$("#resourceToDeleteShown").text(decodeURIComponent(decodeURI(clickedBtn.data("resource"))));
			$("#resourceToDelete").val(clickedBtn.data("resource"));
			getByID("deleteConfirmModal").modal();
		});
	});
			
			
});