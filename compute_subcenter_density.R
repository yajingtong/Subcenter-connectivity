#!/usr/bin/env Rscript
# Road length inside each polygon, divided by its polygon area.
suppressPackageStartupMessages({library(sf);library(dplyr)})
arg<-grep('^--file=',commandArgs(),value=TRUE)
setwd(normalizePath(file.path(dirname(sub('^--file=','',arg[1])),'../..')))
out<-'outputs/subcentre_road_network_density'
dir.create(file.path(out,'by_city'),recursive=TRUE,showWarnings=FALSE)
manifest<-read.csv('scripts/all_cities_local_node_connectivity/road_sources.csv')
patches<-st_read('outputs/subcentre_standardized_poi_rcoi_32_cities/All_32_cities_standardized_POI_RCOI.gpkg',layer='classified_patches',quiet=TRUE) |> st_transform(2154)
counts<-table(patches$city)
cities<-names(counts[counts>5])
stopifnot(all(cities %in% manifest$city))
result<-list();geometry<-list()
for(city_name in cities){
 message('Computing ',city_name)
 m<-manifest[manifest$city==city_name,]
 roads<-st_read(m$road_path,layer=m$road_layer,quiet=TRUE) |> st_transform(2154)
 p<-patches[patches$city==city_name,]
 p<-p[order(as.integer(sub('C','',p$patch_label))),]
 stopifnot(all(st_is_valid(p)),!any(st_is_empty(p)))
 rows<-list()
 for(i in seq_len(nrow(p))){
  ids<-st_intersects(p[i,],roads)[[1]]
  total<-0
  if(length(ids)){
   clipped<-suppressWarnings(st_intersection(st_geometry(roads)[ids],st_geometry(p[i,])))
   clipped<-clipped[!st_is_empty(clipped)&st_dimension(clipped)==1]
   # Dissolve coincident linework so duplicate source ways are not counted twice.
   if(length(clipped))total<-as.numeric(st_length(st_union(clipped)))
  }
  area<-as.numeric(st_area(p[i,]))/1e6
  rows[[i]]<-data.frame(city=city_name,subcentre=p$patch_label[i],patch_id=p$hh_patch_id[i],
   polygon_area_km2=area,road_length_km=total/1000,road_density_km_per_km2=total/1000/area)
 }
 tab<-bind_rows(rows)
 existing<-read.csv(file.path('outputs/all_cities_gt5_local_node_connectivity',city_name,'subcentre_local_node_measures.csv'))
 tab<-left_join(tab,existing[,c('patch_id','street_nodes_excluding_degree2','junctions_degree3plus','street_node_density_per_km2','junction_density_per_km2')],by='patch_id')
 stopifnot(nrow(tab)==nrow(p),!anyNA(tab),all(tab$road_density_km_per_km2>=0))
 result[[city_name]]<-tab
 geometry[[city_name]]<-st_sf(tab,geometry=st_geometry(p))
 write.csv(tab,file.path(out,'by_city',paste0(city_name,'_subcentre_road_density.csv')),row.names=FALSE)
}
tab<-bind_rows(result)
stopifnot(nrow(tab)==89,n_distinct(tab$city)==6,!anyDuplicated(tab$patch_id))
write.csv(tab,file.path(out,'all_subcentre_road_density.csv'),row.names=FALSE)
st_write(bind_rows(geometry),file.path(out,'subcentre_road_density.gpkg'),layer='subcentre_road_density',delete_layer=TRUE,quiet=TRUE)
summary<-tab |> group_by(city) |> summarise(subcentres=n(),total_polygon_area_km2=sum(polygon_area_km2),
 total_road_length_km=sum(road_length_km),pooled_road_density_km_per_km2=total_road_length_km/total_polygon_area_km2,
 median_subcentre_road_density=median(road_density_km_per_km2),.groups='drop')
write.csv(summary,file.path(out,'city_summary.csv'),row.names=FALSE)
lines<-c('# Road-network density by subcentre',
'Road density = road length within the polygon (km) / polygon area (km²).',
'The six cities are those covered by the existing >5-subcentre street-network analysis. Node and junction densities are included separately in the CSV.',
'', '| City | Subcentre | Area (km²) | Road length (km) | Road density (km/km²) |', '|---|---|---:|---:|---:|')
lines<-c(lines,vapply(seq_len(nrow(tab)),function(i)sprintf('| %s | %s | %.3f | %.3f | %.3f |',tab$city[i],tab$subcentre[i],tab$polygon_area_km2[i],tab$road_length_km[i],tab$road_density_km_per_km2[i]),character(1)))
writeLines(lines,file.path(out,'ROAD_DENSITY_TABLE.md'))
writeLines(c('Road length density, not node density: unique road-line length inside each subcentre divided by polygon area. Units km/km² (equivalent to km^-1).',
'Uses the same patches and 14 highway categories as the existing six-city local-node analysis, including service roads and link roads; footpaths and railways are not added.',
'Roads and polygons are projected to Lambert-93 EPSG:2154. Roads are geometrically intersected with each polygon; outside portions are excluded. Exact coincident linework is unioned to avoid duplicate counting.',
'Separate parallel carriageways remain separate lines. No artificial connector lengths, routing distances or whole-road lengths extending outside the polygon are included.',
'Existing full-network street-node and junction densities are joined by patch ID for comparison; they have units nodes/km² and are not used to compute road-length density.',
'City summary density uses pooled road length / pooled subcentre polygon area, not the complete city area.',
'Source manifest: scripts/all_cities_local_node_connectivity/road_sources.csv. Existing inputs and node analysis outputs are unchanged.'),file.path(out,'README.md'))
print(summary)
