

# get_baidu_geocode -------------------------------------------------------------------------

get_baidu_geocode <- function(address, api_key) {
  # Construct the request URL
  url <- paste0(
    "http://api.map.baidu.com/geocoding/v3/?",
    "address=", URLencode(address),
    "&output=json",
    "&ak=", api_key
  )
  
  # Send the GET request
  response <- GET(url)
  
  # Parse response
  raw_content <- content(response, as = "text", encoding = "UTF-8")
  print(raw_content) # Debugging
  
  if (response$status_code == 200) {
    data <- fromJSON(raw_content, flatten = TRUE)
    if (data$status == 0) {
      # Extract longitude, latitude, level, precise, and confidence
      lng <- data$result$location$lng
      lat <- data$result$location$lat
      level <- data$result$level
      precise <- data$result$precise
      confidence <- data$result$confidence
      return(data.frame(
        address = address,
        longitude = lng,
        latitude = lat,
        level = level,
        precise = precise,
        confidence = confidence,
        stringsAsFactors = FALSE
      ))
    } else {
      warning(paste("Error:", data$msg, "for address:", address))
      return(data.frame(
        address = address,
        longitude = NA,
        latitude = NA,
        level = NA,
        precise = NA,
        confidence = NA,
        stringsAsFactors = FALSE
      ))
    }
  } else {
    stop("HTTP request failed with status code: ", response$status_code)
  }
}



get_baidu_geocode <- function(address, api_key) {
  if (is.null(api_key) || api_key == "") {
    stop("API key is missing or invalid.")
  }
  
  # Construct the request URL
  url <- paste0(
    "http://api.map.baidu.com/geocoding/v3/?",
    "address=", URLencode(address),
    "&output=json",
    "&ak=", api_key
  )
  
  # Send the GET request
  response <- GET(url)
  
  # Parse response
  raw_content <- content(response, as = "text", encoding = "UTF-8")
  if (response$status_code == 200) {
    data <- fromJSON(raw_content, flatten = TRUE)
    if (data$status == 0) {
      return(data.frame(
        address = address,
        longitude = data$result$location$lng,
        latitude = data$result$location$lat,
        level = data$result$level,
        precise = data$result$precise,
        confidence = data$result$confidence,
        stringsAsFactors = FALSE
      ))
    } else {
      warning(paste("Geocoding error:", data$message, "for address:", address))
      return(NULL)
    }
  } else {
    stop("HTTP request failed with status code: ", response$status_code)
  }
}

results2 <- results2 %>%
  mutate(
    geocode = map(address, ~{
      Sys.sleep(0.2)  # Add a 0.2 second delay (5 requests per second)
      tryCatch(
        get_baidu_geocode(.x, api_key),
        error = function(e) {
          warning(paste("Error processing address:", .x))
          return(data.frame(
            address = .x,
            longitude = NA,
            latitude = NA,
            level = NA,
            precise = NA,
            confidence = NA,
            stringsAsFactors = FALSE
          ))
        }
      )
    })
  )
results2 <- results2[, c(1:8)]
data <- rbind(results, results2)




get_baidu_geocode <- function(address, api_key) {
  url <- paste0(
    "http://api.map.baidu.com/geocoding/v3/?",
    "address=", URLencode(address),
    "&output=json",
    "&ak=", api_key
  )
  
  response <- GET(url)
  raw_content <- content(response, as = "text", encoding = "UTF-8")
  print(raw_content) # Debugging
  
  if (response$status_code == 200) {
    data <- fromJSON(raw_content, flatten = TRUE)
    if (data$status == 0) {
      return(data.frame(
        address = address,
        longitude = data$result$location$lng,
        latitude = data$result$location$lat,
        level = data$result$level,
        precise = data$result$precise,
        confidence = data$result$confidence,
        stringsAsFactors = FALSE
      ))
    } else {
      warning(paste("Geocoding error:", data$message, "for address:", address))
    }
  } else {
    warning(paste("HTTP error with status code:", response$status_code, "for address:", address))
  }
  
  # Return NA for failed cases
  return(data.frame(
    address = address,
    longitude = NA,
    latitude = NA,
    level = NA,
    precise = NA,
    confidence = NA,
    stringsAsFactors = FALSE
  ))
}

results2 <- results2 %>%
  mutate(
    geocode = map(address, ~{
      Sys.sleep(0.2)  # Add a 0.2 second delay (5 requests per second)
      tryCatch(
        get_baidu_geocode(.x, api_key),
        error = function(e) {
          warning(paste("Error processing address:", .x))
          return(data.frame(
            address = .x,
            longitude = NA,
            latitude = NA,
            level = NA,
            precise = NA,
            confidence = NA,
            stringsAsFactors = FALSE
          ))
        }
      )
    })
  )


get_baidu_geocode <- function(address, api_key) {
  # Construct the request URL
  url <- paste0(
    "http://api.map.baidu.com/geocoding/v3/?",
    "address=", URLencode(address),
    "&output=json",
    "&ak=", api_key
  )
  
  # Send the GET request
  response <- GET(url)
  
  # Print raw response for debugging
  raw_content <- content(response, as = "text", encoding = "UTF-8")
  print(raw_content) # Debugging
  
  # Check HTTP response status
  if (response$status_code == 200) {
    # Parse JSON response
    data <- fromJSON(raw_content, flatten = TRUE)
    
    # Check Baidu API status
    if (data$status == 0) {
      # Extract longitude and latitude
      lng <- data$result$location$lng
      lat <- data$result$location$lat
      return(data.frame(address = address, longitude = lng, latitude = lat, stringsAsFactors = FALSE))
    } else {
      warning(paste("Error:", data$msg, "for address:", address))
      return(data.frame(address = address, longitude = NA, latitude = NA, stringsAsFactors = FALSE))
    }
  } else {
    stop("HTTP request failed with status code: ", response$status_code)
  }
}

get_baidu_geocode("北京市东城区", api_key)


# -------------------------------------------------------------------------



api_key <- "dBwn2Bs6djXRj7n9BAMUpXbMu4jp6Qw1"  # 确保替换为真实的百度地图 API Key

get_baidu_geocode <- function(address, api_key) {
  # Construct the request URL
  url <- paste0(
    "http://api.map.baidu.com/geocoding/v3/?",
    "address=", URLencode(address),
    "&output=json",
    "&ak=", api_key
  )
  
  # Send the GET request
  response <- GET(url)
  
  # Print raw response for debugging
  raw_content <- content(response, as = "text", encoding = "UTF-8")
  print(raw_content) # Debugging
  
  # Check HTTP response status
  if (response$status_code == 200) {
    # Parse JSON response
    data <- fromJSON(raw_content, flatten = TRUE)
    
    # Check Baidu API status
    if (data$status == 0) {
      # Extract longitude and latitude
      lng <- data$result$location$lng
      lat <- data$result$location$lat
      return(data.frame(address = address, longitude = lng, latitude = lat, stringsAsFactors = FALSE))
    } else {
      warning(paste("Error:", data$msg, "for address:", address))
      return(data.frame(address = address, longitude = NA, latitude = NA, stringsAsFactors = FALSE))
    }
  } else {
    stop("HTTP request failed with status code: ", response$status_code)
  }
}

get_baidu_geocode("北京市东城区", api_key)


# -------------------------------------------------------------------------


library(dplyr)
library(purrr)
library(tidyr)
library(jsonlite)

get_baidu_geocode <- function(address, api_key) {
  # Construct the request URL
  url <- paste0(
    "http://api.map.baidu.com/geocoding/v3/?",
    "address=", URLencode(address),
    "&output=json",
    "&ak=", api_key
  )
  
  # Send the GET request
  response <- GET(url)
  
  # Parse response
  raw_content <- content(response, as = "text", encoding = "UTF-8")
  print(raw_content) # Debugging
  
  if (response$status_code == 200) {
    data <- fromJSON(raw_content, flatten = TRUE)
    if (data$status == 0) {
      # Extract longitude, latitude, level, precise, and confidence
      lng <- data$result$location$lng
      lat <- data$result$location$lat
      level <- data$result$level
      precise <- data$result$precise
      confidence <- data$result$confidence
      return(data.frame(
        address = address,
        longitude = lng,
        latitude = lat,
        level = level,
        precise = precise,
        confidence = confidence,
        stringsAsFactors = FALSE
      ))
    } else {
      warning(paste("Error:", data$msg, "for address:", address))
      return(data.frame(
        address = address,
        longitude = NA,
        latitude = NA,
        level = NA,
        precise = NA,
        confidence = NA,
        stringsAsFactors = FALSE
      ))
    }
  } else {
    stop("HTTP request failed with status code: ", response$status_code)
  }
}


# addresses <- data.frame(
#   id = 1:2,
#   address = c("吉林省长春市南关区曙光街道珲春社区平泉小区2-7-401", "北京市海淀区中关村大街59号"),
#   stringsAsFactors = FALSE
# )
# names(address)
# results <- addresses %>%
#   mutate(geocode = map(address, ~ get_baidu_geocode(.x, api_key))) %>% # Apply function to each address
#   unnest(cols = geocode, names_sep = "_") # Add a separator to avoid name duplication
# Replace with your actual Baidu API key
api_key <- "dBwn2Bs6djXRj7n9BAMUpXbMu4jp6Qw1"

# Extract geocodes
names(address)
results1 <- address %>%
  mutate(geocode = map(address, ~ get_baidu_geocode(.x, api_key))) %>% # Apply function to each address
  unnest(cols = geocode, names_sep = "_") # Add a separator to avoid name duplication

results <- results1[!is.na(results1$geocode_confidence),]

results2 <- results1[is.na(results1$geocode_confidence), c(1, 2)]
results2 <- results2 %>%
  mutate(geocode = map(address, ~ get_baidu_geocode(.x, api_key))) %>% # Apply function to each address
  unnest(cols = geocode, names_sep = "_") # Add a separator to avoid name duplication

# View the results
print(results)

library(purrr)
geocode_results <- map(addresses, ~{
  Sys.sleep(0.2)  # 每秒最多 5 个请求，取决于配额
  get_baidu_geocode(.x, api_key)
})



# -------------------------------------------------------------------------

get_baidu_geocode_retry <- function(address, api_key, max_retries = 2, delay = 3) {
  for (i in 1:max_retries) {
    # 尝试获取经纬度
    result <- tryCatch(
      get_baidu_geocode(address, api_key),
      error = function(e) {
        warning(paste("Error during geocoding for address:", address, "- Attempt:", i))
        return(data.frame(
          address = address,
          longitude = NA,
          latitude = NA,
          level = NA,
          precise = NA,
          confidence = NA,
          stringsAsFactors = FALSE
        ))
      }
    )
    
    # 检查结果是否有效
    if (!is.null(result) && !is.na(result$longitude[1]) && !is.na(result$latitude[1])) {
      # 成功后打印提示信息
      message(paste("Successfully processed address:", address, 
                    "-> Longitude:", result$longitude[1], 
                    "Latitude:", result$latitude[1]))
      return(result)  # 如果结果有效，直接返回
    }
    
    # 如果是第一次尝试失败，等待后再重试
    if (i < max_retries) {
      Sys.sleep(delay)  # 延迟重试
      warning(paste("Retrying address:", address, "- Attempt:", i))
    }
  }
  
  # 如果重试多次后仍失败，返回占位结果
  warning(paste("Failed to retrieve geocode after", max_retries, "attempts for address:", address))
  return(data.frame(
    address = address,
    longitude = NA,
    latitude = NA,
    level = NA,
    precise = NA,
    confidence = NA,
    stringsAsFactors = FALSE
  ))
}





