##******************************************************************************
## @file    Dendro2d.R
## @author  Alexandre Arsenault <alx.arsenault@gmail.com>
## @date    10/07/2015
##******************************************************************************

GLOBAL_LINE_WIDTH = 0.55;

CombineRowAndColMergeAndHeight <- function(size, row, col, row_height, col_height)
{
    merge_n_row = ((size[1] -1) + (size[2] -1))
    
    index_row = 1
    index_col = 1
    index_row_two = 1
    index_col_two = 1
    index = 1
    
    delta = 0
    data = vector(mode="numeric", length=0)
    
    for(i in 1:merge_n_row)
    {
        r_height_delta = 0;
        
        if(index_row > (size[1]-1))
        {
            r_height_delta = 1000000;
        }
        else
        {
            r_height_delta = row_height[index_row];
        }
        
        c_height_delta = 1000000000;
        
        if(index_col < size[2])
        {
            c_height_delta = col_height[index_col];
        }
        
        if(r_height_delta < c_height_delta)
        {
            data[index] = row[index_row_two]
            data[index+1] = row[index_row_two+1]
            data[index+2] = row_height[index_row]
            data[index+3] = 0
            
            index_row <- index_row + 1
            index_row_two <- index_row_two + 2
            #delta <- r_height_delta
            index <- index + 4
        }
        else
        {
            data[index] = col[index_col_two]
            data[index+1] = col[index_col_two+1]
            data[index+2] = col_height[index_col]
            data[index+3] = 1
            
            index_col <- index_col + 1
            index_col_two <- index_col_two + 2
            index <- index + 4
        }
    }
    
    merge_matrix = matrix(data,
    nrow = merge_n_row,
    ncol = 4,
    byrow = TRUE) # fill matrix by rows
    
    return(merge_matrix)
}

hbiclust_Alex <- function(data)
{
    d <- dist(data, method = "euclidean");
    v <- hclust(d, method = "complete");
    
    row_merge = as.vector(t(v$merge));
    row_order = v$order;
    row_height = v$height;
    
    d2 <- dist(t(data), method = "euclidean");
    v2 <- hclust(d2, method = "complete");
    
    col_merge = as.vector(t(v2$merge));
    col_order = v2$order;
    col_height = v2$height;
    
    info_matrix <- CombineRowAndColMergeAndHeight(dim(data),
                                                  row_merge,
                                                  col_merge,
                                                  row_height,
                                                  col_height);
    
    size = dim(data);
    total_length = size[1] + size[2] - 2;
    l_vec = 1:total_length;
    
    merge_matrix <- info_matrix[l_vec, -c(3, 4)]
    height_vector <- info_matrix[l_vec, -c(1, 2, 4)]
    rowcol_vector <- info_matrix[l_vec, -c(1, 2, 3)]
    
    newList <- list("row_col" = rowcol_vector,
                    "merge" = merge_matrix,
                    "height" = height_vector,
                    "dim" = size,
                    "row_order" = row_order,
                    "col_order" = col_order,
                    "row_name" = rownames(data),
                    "col_name" = colnames(data),
                    "data" = data);
    
    class(newList) <- "hbiclust";

    return(newList);
}

DrawBottomRect <- function(size, size_ratio, margin)#, col_limits, row_limits)
{
    x_left = margin[2];
    x_right = margin[2] + size_ratio[2];
    
    y_bottom = margin[1];
    y_top = margin[1] + size_ratio[1];
    
    
    # Row.
    for( k in seq(1, size[1] - 1) )
    {
        y_pos = margin[1] + (k / size[1]) * size_ratio[1];
        segments(x_left, y_pos, x_right, y_pos, col="lightgray", lty=3);        
    }
    
    # Col.
    for( k in seq(1, size[2] - 1) )
    {
        x_pos = margin[2] + (k / size[2]) * size_ratio[2];
        segments(x_pos, y_bottom, x_pos, y_top, col="lightgray", lty=3);
    }
        
    # Draw contour.
    rect(x_left, y_bottom, x_right, y_top, col=NA, border="black");
}



MergeMatrix <- function(size, data)
{
    merge_n_row = ((size[1] -1) + (size[2] -1))
    
    merge_matrix = matrix(data,
    nrow = merge_n_row,
    ncol=2,
    byrow = TRUE) # fill matrix by rows
    
    return(merge_matrix);
}

SplitVectors <-function(size, merge_matrix, height_vector, rowcol_vector)
{
    row_merge = vector(mode="numeric", length=(size[1] - 1) * 2);
    row_hight = vector(mode="numeric", length=size[1] - 1);
    
    col_merge = vector(mode="numeric", length=(size[2] - 1) * 2);
    col_hight = vector(mode="numeric", length=size[2] - 1);
    
    total_merges = ((size[1] -1) + (size[2] -1));
    index_row_merge = 0;
    index_col_merge = 0;
    
    n_merge = 0;
    
    for(i in 1:total_merges)
    {
        if(rowcol_vector[i] == 0)
        {
            row_merge[index_row_merge * 2 + 1] = merge_matrix[n_merge + 1, 1];
            row_merge[index_row_merge * 2 + 2] = merge_matrix[n_merge + 1, 2];
            row_hight[index_row_merge + 1] = height_vector[n_merge + 1];
            
            index_row_merge = index_row_merge + 1;
            
        } else {
            col_merge[index_col_merge * 2 + 1] = merge_matrix[n_merge + 1, 1];
            col_merge[index_col_merge * 2 + 2] = merge_matrix[n_merge + 1, 2];
            col_hight[index_col_merge + 1] = height_vector[n_merge + 1];
            
            index_col_merge = index_col_merge + 1;
        }
        
        n_merge = n_merge + 1;
    }
    
    return(list(row_merge = matrix(row_merge, nrow = size[1] - 1, ncol = 2, byrow = TRUE),
				row_hight = row_hight,
                col_merge = matrix(col_merge, nrow = size[2] - 1, ncol = 2, byrow = TRUE),
                col_hight = col_hight));
}

DrawRowDendrogram <-function(size, merge_info, size_ratio, order, margin, cut_height)
{
    n_row_merge = size[1] - 1;
    answer = ProcessRowDendrogram(n_row_merge,
    merge_info$row_merge,
    merge_info$row_hight,
    order,
    cut_height);
    
    lines = answer$lines;
    
    odd_i = c(1, 3, 5, 7, 9, 11);
    even_i = c(2, 4, 6, 8, 10, 12);

    lines[, odd_i] = lines[, odd_i] * (1.0 - (size_ratio[2] + margin[2]));
    lines[, odd_i] = lines[, odd_i] + size_ratio[2] + margin[2];
    
    lines[, even_i] = lines[, even_i] * size_ratio[1] + margin[1];
    
    lwd = GLOBAL_LINE_WIDTH;

    for(l in 1:n_row_merge)
    {
        lc = rgb(lines[l, 13], lines[l, 14], lines[l, 15]);

        segments(lines[l, 1], 
                 lines[l, 2], 
                 lines[l, 3], 
                 lines[l, 4], 
                 col=lc, lwd=lwd);

        segments(lines[l, 5], 
                 lines[l, 6], 
                 lines[l, 7], 
                 lines[l, 8], 
                 col=lc, lwd=lwd);

        segments(lines[l, 9], 
                 lines[l, 10], 
                 lines[l, 11], 
                 lines[l, 12], 
                 col=lc, lwd=lwd);
    }
    
    # Draw cut line.
    norm_cut_height = cut_height / merge_info$row_hight[size[1] - 1];
    y_bottom = margin[1];
    y_top = margin[1] + size_ratio[1];
    x_pos = norm_cut_height * (1.0 - (size_ratio[2] + margin[2])) + size_ratio[2] + margin[2];
    
    segments(x_pos, y_bottom, x_pos, y_top, col=rgb(0.5, 0.5, 0.5), lty=3);
    
    return(answer$limits);
}

DrawColDendrogram <-function(size, 
                             merge_info, 
                             size_ratio, 
                             order, 
                             margin, 
                             cut_height)
{
    n_col_merge = size[2] - 1;

    answer = ProcessColDendrogram(n_col_merge, 
                                  merge_info$col_merge, 
                                  merge_info$col_hight, 
                                  order, 
                                  cut_height);
    lines = answer$lines;
    limits = answer$limits;
    
    odd_i = c(1, 3, 5, 7, 9, 11);
    even_i = c(2, 4, 6, 8, 10, 12);
    lines[, odd_i] = lines[, odd_i] * size_ratio[2] + margin[2];
    lines[, even_i] = lines[, even_i] * (1.0 - (size_ratio[1] + margin[1]));
    lines[, even_i] = lines[, even_i] + size_ratio[1] + margin[1];
    
    lwd = GLOBAL_LINE_WIDTH;

    for(l in 1:n_col_merge)
    {
        lc = rgb(lines[l, 13], lines[l, 14], lines[l, 15]);

        segments(lines[l, 1], 
                 lines[l, 2], 
                 lines[l, 3], 
                 lines[l, 4], 
                 col=lc, lwd=lwd);

        segments(lines[l, 5], 
                 lines[l, 6], 
                 lines[l, 7], 
                 lines[l, 8], 
                 col=lc, lwd=lwd);

        segments(lines[l, 9], 
                 lines[l, 10], 
                 lines[l, 11], 
                 lines[l, 12], 
                 col=lc, lwd=lwd);
    }
    
    # Draw cut line.
    norm_cut_height = cut_height / merge_info$col_hight[size[2] - 1];
    x_left = margin[2];
    x_right = margin[2] + size_ratio[2];
    y_pos = norm_cut_height * (1.0 - (size_ratio[1] + margin[1]));
    y_pos = y_pos + size_ratio[1] + margin[1];
    
    segments(x_left, y_pos, x_right, y_pos, col=rgb(0.5, 0.5, 0.5), lty=3);
    
    return(limits);
}

DrawRowNames <- function(size, size_ratio, names, margin)
{
    # y_pos = 0.0;
    x_pos = 0.0;
    y_ratio = (1.0 / size[1]) * size_ratio[1];
    half_size = y_ratio * 0.5;
    
    for(i in seq(1, size[1]))
    {
        y_pos = (i - 1) * y_ratio + half_size + margin[1];
        
        text(x_pos, y_pos, labels=names[i], cex=0.5);
    }
}

DrawColNames <- function(size, size_ratio, names, margin)
{
    y_pos = 0.0;
    x_ratio = (1.0 / size[2]) * size_ratio[2];
    half_size = x_ratio * 0.5;
    
    for(i in seq(1, size[2]))
    {
        x_pos = (i - 1) * x_ratio + half_size + margin[2];
        
        text(x_pos, y_pos, labels=names[i], cex=0.5, srt=270);
    }
}

hbiclust.plot <- function(clust_info, 
                          size_ratio = c(0.7, 0.7), 
                          cut_height = 9.0,
                          n_cluster_color = 0)
{
    # Using nbcairo or cairo instead of Xlib add the support for transparency.
    #x11(type = "nbcairo", antialias = "subpixel");
    plot.new();

    margin = c(0.07, 0.04);
    size = clust_info$dim;

    DrawRowNames(size, size_ratio, clust_info$row_name[clust_info$row_order], margin);
    DrawColNames(size, size_ratio, clust_info$col_name[clust_info$col_order], margin);
    
    # Split row and column data info.
    merge_info = SplitVectors(size, 
                              clust_info$merge, 
                              clust_info$height, 
                              clust_info$row_col);
    
    # Draw row dendrogram and return row limits for cut rectangle.
    row_limits = DrawRowDendrogram(size, 
                                   merge_info, 
                                   size_ratio, 
                                   clust_info$row_order, 
                                   margin, 
                                   cut_height);

    # Draw column dendrogram and return colum limits for cut rectangle.
    col_limits = DrawColDendrogram(size, 
                                   merge_info, 
                                   size_ratio, 
                                   clust_info$col_order, 
                                   margin, 
                                   cut_height);
    
    # Draw base rectangle.
    DrawBottomRect(size, size_ratio, margin);
    
    
    clust2d = ProcessClusterOnRect(size, 
                                   size_ratio, 
                                   margin, 
                                   row_limits, 
                                   col_limits,
                                   n_cluster_color);

    rect_points = clust2d$rect_points;
    
    for(i in seq(1, clust2d$n_clust))
    {
        rect_color = rgb(rect_points[i, 5], 
                         rect_points[i, 6], 
                         rect_points[i, 7], 
                         alpha=0.3);

        rect(rect_points[i, 1], 
             rect_points[i, 2], 
             rect_points[i, 3], 
             rect_points[i, 4], 
             col = rect_color, 
             lwd = GLOBAL_LINE_WIDTH);
    }    
}

