# Eden_valley_cattle
downloaded collar data
analysis was started with a different branch.
I have had lots of problems, the speed on the anlysis is one.
Running this is very slow.
The best solution is running the scripts in Pearcy.
I am not sure why (might be a version thing but some content of the markdown file doesnt work).

I have a number of markdown files that I have now transfered back to scripts.
This is something I would like to work on later, having a Rmarkdown file even if it refernces scripts that run on pearcey.

The other problem I have is the placement of the VF.
one fence was placed along the road and this data should be excluded.
I have now disregarded the value clm in the collar data and recalulated the distance from the VF.

I have done this by dividing the data up into VF days - the start and end of each fence.
Then using spatial tools I have wokred out the distance from the fence, specific for each fence.
This hinges on the correct dates and times.

I am unsure how to proceede with the cue data - should it just be deleted from the analysis, ie all of this fence data removed?
