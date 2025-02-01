# MOVE? to independent file for header rendering and for dumpcwd()
#  &why: .loci is a chain of .locus corresponding to args of `Selector(`Action(.parent()), <args>),
#    meaning `Loci isn't a part of `Entity at all and shouldn't be here.
#    Instead we should keep `SelectorArgs in each `Entity
## FMT::
#  * miur://Providers:FSRoot/Listing:etc/Listing:fstab/TextLines:4/Chars:8
#  * miur://ver:2/FSRoot:FSDir:etc/FSFile:fstab/ByteExtent:422+84
#  * miur://FSRoot:/FSDir:etc/FSFile:fstab/ByteExtent:422+84
#  * miur://FSRoot:/FSDir:etc/FilterBy:^nm/Sorted:size/
#  * miur:file:///etc/fstab
class Loci:
    pass
