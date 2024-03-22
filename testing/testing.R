library(move)
library(move2)
library(ggplot2)

data(fishers)
fishers2 <- mt_as_move2(fishers)

#get osm
osmdata <- hi_get_osm(fishers2)
osmdata_railway <- hi_get_osm(fishers2,key='railway')

#distance
fishers_d <- hi_distance(fishers2)
boxplot(nearest_distance ~ nearest_value, fishers_d)
fishers_b <- hi_distance(fishers2,key='building',geom='polygon')
boxplot(nearest_distance ~ nearest_value, fishers_b)

#crossing
fishers_c <- hi_crossing(fishers2)
table(fishers_c$crossing_value)

#crossing loc
x2 <- hi_crossing_loc(fishers2,crs_code=32618)  ## takes 12 seconds
ggplot() + geom_sf(data=x2, aes(color = value)) + coord_sf()

#check bbox
fishers_bb <- hi_check_bbox(fishers2)
ggplot() + geom_sf(data=fishers_bb, aes(color = trackId)) + coord_sf()

#buffer
fishers_buf <- hi_buffer(fishers2,r=50,crs_code=32618)
ggplot() + geom_sf(data=fishers_buf, aes(color = buf_code)) + coord_sf()
buf_50 <- hi_buffer(fishers2,r=50,crs_code=32618,return="buffer")
ggplot() + geom_sf(data=fishers_buf, aes(color = track)) + geom_sf(data=buf_50) + coord_sf()

#by id
fishers_d <- hi_by_trackId(fishers2,fun="hi_distance", key="highway", value="track") 
