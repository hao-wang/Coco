program Coco
!	use cosmoSetUp
	use subroutines
	use paramScan
	use GenerateIDL
!	use IO
	
	use_MCMC=0
	call ioInit

	call SetModelParam
	call CheckParam

	call ReadInObsData
	call GenIDL(use_MCMC)

	call ScanParams(use_MCMC)
	call freeMemory
	call ioEnd
end program
