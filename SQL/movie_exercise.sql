--select * from dbo.movies_genres;

--select * from dbo.movie_collection order by collection_id asc;

--select * from dbo.movie_collection where collection_id = 84;

--select * from dbo.collection_dim;

--select * from dbo.movie_keywords;

--select * from dbo.keywords_dim;



--select count(1) cnt, keyword_id from dbo.movie_keywords mov group by (keyword_id) order by (cnt) desc;

-- keywords_dim kw where mov.keyword_id = kw.keyword_id group by kw.keyword_id;


--select * from dbo.movie_cast where actor_id = 52997 and "order" < 3;

--select * from dbo.movie_overview;


--countries
select * from (select  iso_3166_1, movie.movie_id from dbo.movies movie left join dbo.movie_countries country on movie.movie_id = country.movie_id) as source_table
pivot (count(iso_3166_1) for iso_3166_1 in ([US],[GB],[FR],[DE],[CA],[IN],[IT],[JP],[AU],[ES])) as MovieCoutries order by movie_id desc

--languages
select * from (select  iso_639_1, movie.movie_id from dbo.movies movie left join dbo.movie_languages mov_language on movie.movie_id = mov_language.movie_id) as source_table
pivot (count(iso_639_1) for iso_639_1 in ([en],[fr],[es],[de],[ru],[it],[ja],[zh],[hi],[ar])) as MovieLanguages order by movie_id desc


--select count(sw_original_lang) cnt, sw_original_lang  from dbo.movie_languages group by sw_original_lang












