package com.example.demo.controllers;

import java.net.URLEncoder;
import java.util.HashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.example.demo.client.ApiClient;
import com.example.demo.client.ApiConfig;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

@JsonRootName(value = "data")
class Film {
	public int filmId;
	public String title;
	public String description;
	public float rentalRate;
	public String lastUpdate;
	
	@JsonCreator
	public Film(
		@JsonProperty("film_id") int filmId,
		@JsonProperty("title") String title,
		@JsonProperty("description") String description,
		@JsonProperty("rental_rate") float rentalRate,
		@JsonProperty("last_update") String lastUpdate) {
		this.filmId = filmId;
		this.title = title;
		this.description = description;
		this.rentalRate = rentalRate;
		this.lastUpdate = lastUpdate;
	}

	@Override
	public String toString() {
		return "Film [description=" + description + ", filmId=" + filmId + ", lastUpdate=" + lastUpdate + ", rentalRate="
				+ rentalRate + ", title=" + title + "]";
	}
}

class UpdateFilmRequest {
	public String title;
	public String description;
	public String rental_rate;
	public String last_update;
	
	@JsonCreator
	public UpdateFilmRequest(
		@JsonProperty("title") String title,
		@JsonProperty("description") String description,
		@JsonProperty("rental_rate") Float rentalRate,
		@JsonProperty("last_update") String lastUpdate) {
		this.title = title;
		this.description = description;
		this.rental_rate = rentalRate.toString();
		this.last_update = lastUpdate;
	}

}

class FilmSearchRequest {
	public String requestParamL;
	
	public FilmSearchRequest(
		String filter,
		int take,
		int skip
	) {
		String requestParam = "";
		if (take < 0) {
			take = 16;
		}
		requestParam = requestParam+"take="+take;
	
		if (skip < 0) {
			skip = 0;
		}
		requestParam = requestParam+"&skip="+skip;

		if (filter != null && filter.length() > 0) {
			String newParam = "{'OR':[{'title':{'contains':'"+filter+"'}},{'description':{'contains':'"+filter+"'}}]}";
			newParam = newParam.replace("'", "\"");

			try {
				newParam = URLEncoder.encode(newParam, "UTF-8");
				System.out.println("Request Param: "+newParam);
				requestParam = requestParam+"&filter="+newParam;
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		requestParamL = requestParam;
	}
}

class FilmSearchResponse {
	public Film[] data;
	
	@JsonCreator
	public FilmSearchResponse(
		@JsonProperty("data") Film[] data) {
		this.data = data;
	}
}

@Controller
public class FilmController {
	@Autowired
    public ApiConfig apiConfig;

    @RequestMapping(value = "/films", method = RequestMethod.GET)
	public String searchFilms(
		Model model,
		@RequestParam(required = false, defaultValue = "") String search,
		@RequestParam(required = false, defaultValue = "16") int take,
		@RequestParam(required = false, defaultValue = "0") int skip) {
		
		FilmSearchRequest request = new FilmSearchRequest(search, take, skip);
		System.out.println("Request: "+request.requestParamL);
		
		ApiClient apiClient = new ApiClient(apiConfig);
		HashMap<Number, String> res = apiClient.call("rest/film", "GET", request.requestParamL);
		
		String responseString = res.getOrDefault(200, null);
		
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

		if (responseString == null) {
			model.addAttribute("errors", res.values().toString());
			return "error";
		}

		FilmSearchResponse films = null;
		try {
			films = objectMapper.readerFor(new TypeReference<FilmSearchResponse>(){}).readValue(responseString);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
			model.addAttribute("errors", e.getMessage());
			return "error";
		}

		model.addAttribute("films", films.data);
		model.addAttribute("search", search);
		return "films";
	}

    @RequestMapping(value = "/film/{filmId}", method = RequestMethod.GET)
	public String getFilmById(Model model, @PathVariable Integer filmId) {
		ApiClient apiClient = new ApiClient(apiConfig);
		HashMap<Number, String> res = apiClient.call("rest/film/"+filmId, "GET", null);
		
		String responseString = res.getOrDefault(200, null);

		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.enable(DeserializationFeature.UNWRAP_ROOT_VALUE);
		objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

		if (responseString == null) {
			model.addAttribute("errors", res.values().toString());
			return "error";
		}

		Film film = null;
		try {
			film = objectMapper.readerFor(new TypeReference<Film>(){}).readValue(responseString);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
			model.addAttribute("errors", e.getMessage());
			return "error";
		}

		model.addAttribute("film", film);
		return "film";
	}

	@RequestMapping(value = "/edit-film/{filmId}", method = RequestMethod.GET)
	public String getFilmByIdForEdit(Model model, @PathVariable Integer filmId) {
		ApiClient apiClient = new ApiClient(apiConfig);
		HashMap<Number, String> res = apiClient.call("rest/film/"+filmId, "GET", null);
		
		String responseString = res.getOrDefault(200, null);

		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.enable(DeserializationFeature.UNWRAP_ROOT_VALUE);
		objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

		Film film = null;
		try {
			film = objectMapper.readerFor(new TypeReference<Film>(){}).readValue(responseString);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		if (film == null) {
			return "error";
		}

		model.addAttribute("film", film);
		return "edit-film";
	}

	@RequestMapping(value = "/edit-film/{filmId}", method = RequestMethod.POST)
	public String updateFilmById(Model model, @PathVariable Integer filmId, Film film) {
		UpdateFilmRequest request = new UpdateFilmRequest(film.title, film.description, film.rentalRate, film.lastUpdate);
		ObjectMapper objectMapper = new ObjectMapper();
		
		String requestString = null;
		try {
			objectMapper.setSerializationInclusion(Include.NON_NULL);
			requestString = objectMapper.writeValueAsString(request);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}

		System.out.println("Request String: "+requestString);
		ApiClient apiClient = new ApiClient(apiConfig);
		HashMap<Number, String> res = apiClient.call("rest/film/"+filmId, "PATCH", requestString);
		
		String responseString = res.getOrDefault(200, null);

		objectMapper.enable(DeserializationFeature.UNWRAP_ROOT_VALUE);
		objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

		String errorString = null;

		if (responseString == null) {
			errorString = res.values().toString();
			model.addAttribute("errors", errorString);
			return "error";
		}

		// Redirect to the film page
		return "redirect:/film/"+filmId;
	}
}
