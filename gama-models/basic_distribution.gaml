/**

*/
model basic_distribution

global {
	graph the_graph;
	string graph_type <- "small-world" among: ["small-world", "scale-free", "complete", "random"];
	int nb_nodes <- 10;
	int nb_edges <- 50;
	float p <- 0.2;
	int k <- 4;
	int m <- 4;

	// Voter type distribution
	// Calculating agent percentages 
	float pct_leave <- 0.2;
	float pct_undecided <- 0.3;
	float pct_remain <- 0.5;

	// Calculating agent indexes 
	int leave_index <- floor(nb_nodes * pct_leave);
	int undecided_index <- leave_index + floor(nb_nodes * pct_undecided);

	// Readership percentages
	float pct_readership <- 0.57;

	// Article types percentages
	float pct_article_leave <- 0.48;
	float pct_article_remain <- 0.22;
	float pct_article_mixed <- 0.26;
	float pct_article_neutral <- (1 - pct_article_leave - pct_article_remain - pct_article_mixed);

	// Defining number of read and no read agents in simulation
	int read <- int(floor(nb_nodes * pct_readership));
	int no_read <- nb_nodes - read;

	// Generating placeholder reading lists
	list no_reading_list <- list_with(no_read, "false");

	// Defining number of article type agents in simulation
	int article_leave <- int(floor(read * pct_article_leave));
	int article_remain <- int(floor(read * pct_article_remain));
	int article_mixed <- int(floor(read * pct_article_mixed));
	int article_neutral <- int(read - article_leave - article_remain - article_mixed);

	// Generating placeholder article type list
	list article_leave_list <- list_with(article_leave, "pro-leave");
	list article_remain_list <- list_with(article_remain, "pro-remain");
	list article_mixed_list <- list_with(article_mixed, "mixed");
	list article_neutral_list <- list_with(article_neutral, "neutral");
	list combined_article_list <- article_leave_list + article_remain_list + article_mixed_list + article_neutral_list;

	// Final list for the article position
	list final_list <- shuffle(combined_article_list + no_reading_list);

	// IMPORTANT: define homogeniety neighborhood composition threshold 
	float homogeneous_limit <- 0.75;

	// IMPORTANT: agent and media reflex influx rate
	float media_effect_rate <- 0.01;
	float agent_effect_rate <- 0.01;

	// Percentage of neighbours that think like you (threshold)
	float rate_think_alike <- 0.50;

	// Numbers of days for the simulation. Need to balance with agent_effect_rate
	int days <- int(10 #weeks); // 10-weeks Referendum campaign

	// time = cycle * step
	// cycle is incremented by 1 at each simulation step
	float step <- 1 #day;
	// so our time represents days -> 2nd cycle = 2nd time = 2nd day
	reflex end when: time > days {
		do pause;
	}

	init {
		switch graph_type {
			match "random" {
				the_graph <- generate_random_graph(nb_nodes, nb_edges, true, node_agent, edge_agent);
			}

			match "scale-free" {
				the_graph <- generate_barabasi_albert(int(nb_nodes / 2), 5, nb_nodes, true, node_agent, edge_agent);
			}

			match "small-world" {
				the_graph <- generate_watts_strogatz(nb_nodes, p, k, true, node_agent, edge_agent);
			}

			match "complete" {
				the_graph <- generate_complete_graph(nb_nodes, true, node_agent, edge_agent);
			}

		}

		write the_graph;
		write "Edges : " + length(the_graph.edges);
		write "Nodes : " + length(the_graph.vertices);
	}

}

species edge_agent {

	aspect default {
		draw shape color: #black;
	}

}

species node_agent {

// Voting preference
	float vote <- float(0.00, 1.00) min: 0.00 max: 1.00 with_precision 1;
	rgb agent_color <- #black;
	string voter_type;

	// Article information
	string article_type;
	rgb agent_border <- #black;
	string agent_readership;

	// Network composition 
	bool neighbourhood_thinks_alike;
	bool homogeneous_network;

	// Assigning attributes to each agent: Voting preference
	reflex set_vote when: time = 0 #day {
		if (int(name) < leave_index) {
			vote <- rnd(0.56, 1.00) with_precision 2;
			voter_type <- "pro-leave";
			// Pro-leave color (255,0,0) The red component is set to a constant value of 255, maximum intensity
			// gradiant from red to white	
			agent_color <- rgb(255, 255 * (1 - (vote - 0.5) * 2), 255 * (1 - (vote - 0.5) * 2));
		} else if ((leave_index <= int(name)) and (int(name) < undecided_index)) {
			vote <- rnd(0.45, 0.55) with_precision 2;
			voter_type <- "undecided";
			// white
			agent_color <- rgb(255, 255, 255);
		} else {
			vote <- rnd(0.00, 0.44) with_precision 2;
			voter_type <- "pro-remain";
			// Pro-remain, color (0,0,255) The blue component is set to a constant value of 255, maximum intensity
			// gradiant from blue to white
			agent_color <- rgb(255 * vote * 2, 255 * vote * 2, 255);
		}

	}

	// Assigning whether they read or not the news, and which type of article they read
	reflex set_newspaper when: time = 0 #day {
	// List the size of the agent population respecting the percentage of readers/non-readers
		string readership <- final_list[int(name)];
		if (readership = "false") {
			agent_readership <- "does not read";
			agent_border <- #grey;
			article_type <- "none";
		} else {
			agent_readership <- "reads";
			article_type <- readership;
			if (article_type = "pro-leave") {
				agent_border <- #red;
			}

			if (article_type = "pro-remain") {
				agent_border <- #blue;
			}

			if (article_type = "mixed") {
				agent_border <- #purple;
			}

			if (article_type = "neutral") {
				agent_border <- #orange;
			}

		}

	}

	// Reflex that allows agents to know the vote of their neighbors 	
	reflex color_neighbors {
		float n_leave <- 0.0;
		float n_remain <- 0.0;
		list<node_agent> list_neighbors <- list<node_agent>(the_graph neighbors_of (self));
		if (empty(list_neighbors)) {
			neighbourhood_thinks_alike <- true;
			homogeneous_network <- true;
		} else {
			int nb_neighbours <- length(list_neighbors);
			float nb_neighbours_mean_leave_value <- (list_neighbors sum_of (each.vote) / nb_neighbours);
			if ((nb_neighbours_mean_leave_value > 0.55 and vote > 0.55) or (nb_neighbours_mean_leave_value < 0.45 and vote < 0.45)) {
				neighbourhood_thinks_alike <- true;
			} else {
				neighbourhood_thinks_alike <- false;
			}

			loop i over: list_neighbors {
				if (i.vote > 0.55) {
					n_leave <- n_leave + 1;
				}

				if (i.vote < 0.45) {
					n_remain <- n_remain + 1;
				}

			}

			if (n_leave / nb_neighbours > homogeneous_limit or n_remain / nb_neighbours > homogeneous_limit) {
				homogeneous_network <- true;
			} else {
				homogeneous_network <- false;
			}

		}

	}

	reflex media_effect when: time > 0 #day {
		if (agent_readership = "reads") {
			if (homogeneous_network = true) {
			// Pro-leave voter
				if (vote > 0.55) {
					if (article_type = "pro-leave" and vote < 1) {
						vote <- vote + media_effect_rate with_precision 2;
					}
					// Pro-Remain articles do not have an effect because the neighborhood is homogenous
					// The nodes and the neighbors think alike --> pro-Leave
					if (article_type = "pro-remain" and vote > 0) {
						vote <- vote with_precision 2;
					}

				}
				// Pro-remain voter
				if (vote < 0.45) {
				// pro-Leave articles do not have an effect
					if (article_type = "pro-leave" and vote < 1) {
						vote <- vote with_precision 2;
					}
					// pro-Remain media has an effect, subtracts 0.1 to leave
					if (article_type = "pro-remain" and vote > 0) {
						vote <- vote - media_effect_rate with_precision 2;
					}

				}

			}

			// Heterogenous network
else {
				if (article_type = "pro-leave" and vote < 1) {
					vote <- vote + media_effect_rate with_precision 2;
				}

				if (article_type = "pro-remain" and vote > 0) {
					vote <- vote - media_effect_rate with_precision 2;
				}

			}

		}

		do update;
	}

	reflex agent_effect when: time > 0 #day {
		if (!neighbourhood_thinks_alike) {
			if (vote > 0.55) {
				vote <- vote - agent_effect_rate with_precision 2;
			} else if (vote < 0.45) {
				vote <- vote + agent_effect_rate with_precision 2;
			} else {
			/*  If the voter is exactly 0.5, undecided/neutral, 
			 it does not exactly the color of their neighbors (it only knows if they are like me or not)
			 Decide randomly between both sides, and the correct side will be selected automatically thanks to reinforcing that later */
				vote <- rnd(0.45, 0.55);
			}

		} else if (neighbourhood_thinks_alike) {
			if (vote > 0.55) { // Not including 0.5 because its exactly the equilibrium, remains at 0.5
				vote <- vote + agent_effect_rate with_precision 2;
			} else if (vote < 0.45) {
				vote <- vote - agent_effect_rate with_precision 2;
			}

		}

		do update;
	}

	action update {
		if vote > 0.55 with_precision 2 {
			agent_color <- rgb(255, 255 * (1 - (vote - 0.5) * 2), 255 * (1 - (vote - 0.5) * 2));
			voter_type <- "pro-leave";
		} else if vote < 0.45 with_precision 2 {
			agent_color <- rgb(255 * vote * 2, 255 * vote * 2, 255);
			voter_type <- "pro-remain";
		} else {
			agent_color <- rgb(255, 255, 255);
			voter_type <- "undecided";
		}

	}

	aspect default {
		draw circle(1) color: agent_color border: agent_border;
		draw string(the_graph degree_of self) color: #black size: 4 at: {self.location.x - 1, self.location.y - 2};
	}

}

experiment loadgraph type: gui {
	output {
		display map type: 2d {
			species edge_agent aspect: default;
			species node_agent aspect: default;
		}

	}

}