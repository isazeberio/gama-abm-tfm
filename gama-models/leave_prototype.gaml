/**
* Name: Leave
* Social simulation of the conditinional effect of media on interpersonal discussion and network structure 
* in the context of Brexit Referendum campaign. The model simulates a prototype scenario in which the majority
* of the voter support the Leave position before the start of the campaign (media effect)
* Author: Isabela Zeberio
* Tags: opinion dynamic, interpersonal discussion network
*/

model Leave

global {
	////////////////////////////////////////////////////////////////// modifiable values
	graph the_graph;
	string graph_type <- "small-world" among: ["small-world", "scale-free", "complete", "random"]; // Selected random as default
	string the_layout parameter: true init: "Circle" among: ["Circle", "Forced", "Grid"];
	// initial graph values
	int nb_nodes <- 2500; // People
	int nb_edges <- 2500; // Links between people. Directed max: n*(n - 1) and Undirected max: (n*(n-1))/2
	float p <- 0.2; // Probability to rewire an edge (beta)
	int k <- 4; // Base degree of each node. k must be even
	int m <- 4; // Number of edges added per novel node

	// Voter percentages
	// Substract 0.05 from the first categories to create 10% of undecided voters
	float perc_leave_voters <- 0.6103-0.05;
	float perc_remain_voters <- 0.3897-0.05;
	float perc_undecided_voters <- 0.1; // (not modifiable)

	// Media reach percentages
	float perc_reach_leave_media <- 0.48;
	float perc_reach_remain_media <- 0.22;
	
	// IMPORTANT: define homogeniety neighborhood composition threshold
	float homogeneous_limit <- 0.75;

	// IMPORTANT: threshold defined in leave variable for undecided voter
	float undecided_range_max <- 0.55;
	float undecided_range_min <- 0.45;

	// IMPORTANT: agent and media reflex influx rate
	float media_effect_rate <- 0.01;
	float agent_effect_rate <- 0.01;

	// Percentage of neighbours that think like you (threshold)
	float rate_think_alike <- 0.5;

	// Numbers of days for the simulation. Need to balance with agent_effect_rate
	int days <- int(70 #days); // 10-weeks Referendum campaign

	////////////////////////////////////////////////////////////////// not modifiable values

	// Initial values of n_change counters to keep track of the position changes
	int n_changes_to_remain <- 0;
	int n_changes_to_leave <- 0;
	int n_changes_to_undecided <- 0;

	// Values to plot, necessary to add update based on the impact of media and interpersonal communication mechanisms
	int n_undecided_voters <- int(perc_undecided_voters * nb_nodes) update: node_agent count (each.leave >= undecided_range_min and each.leave <= undecided_range_max);
	int n_leave_voters <- int(perc_leave_voters * nb_nodes) update: node_agent count (each.leave > undecided_range_max);
	int n_remain_voters <- int(perc_remain_voters * nb_nodes) update: node_agent count (each.leave < undecided_range_min);

	// time = cycle * step
	// cycle is incremented by 1 at each simulation step
	float step <- 1 #day; 
	// so our time represents days -> 2nd cycle = 2nd time = 2nd day
	reflex end when: time > days { 
		do pause;
	}


	init {
		// Save initialization data
		// rewrite: false allow us to save the results of different simulations in differnet rows
		save [n_leave_voters, n_remain_voters, n_undecided_voters] to: "initial_values.csv" format: "csv" rewrite: false ;
	
		
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

		// Graph information
		write the_graph;
		write "Edges: " + length(the_graph.edges);
		write "Nodes: " + length(the_graph.vertices);
		write "Mean vertice degree: " + mean(the_graph.vertices collect (the_graph degree_of each));
		write "nb_cycles: " + nb_cycles(the_graph);
		write "alpha_index: " + alpha_index(the_graph);
		write "beta_index: " + beta_index(the_graph);
		write "gamma_index: " + gamma_index(the_graph);
		write "connectivity_index: " + connectivity_index(the_graph);
		write "connected_components_of: " + length(connected_components_of(the_graph));		
		write "maximal_cliques_of:" + (maximal_cliques_of(the_graph) collect (length(each)));
		write "biggest_cliques_of:" + (biggest_cliques_of(the_graph) collect (length(each)));
	}
	
	// Choose the layout of the graph
	action layout_graph {
		write "Graph layout\n";
		switch the_layout {
			match "Circle" {
				do c_layout;
			}
			match "Grid" {
				do g_layout;
			}
			match "Forced" {
				do f_layout;
			}
			
		}

	}
	action c_layout {
			write "Circle classical layout : nodes are randomly placed on a circle";
			the_graph <- layout_circle(the_graph, world.shape, // The geometry to spatialize nodes in 
			false // Shuffle or not the nodes
);
		}

		action f_layout {
			write "Forced based layout : connected node pull each other, while unconnected node push each other away";
			the_graph <- layout_force(the_graph, world.shape, // The geometry to spatialize nodes in 
			0.4, // The pull/push force
			0.01, // The cooling rate of the algorithm
			100 // Maximum number of iterations
);
		}

		action g_layout {
			write "Homemade grid based layout : distributes nodes over a grid to minimize edge crossing";
			graph q <- layout_grid(the_graph, world.shape, // The geometry to spatialize nodes in
			1.5 // The ratio of possible grid position over the total number of nodes (should be higher than 1.0 )
);
		} 

}


species edge_agent {

	aspect default {
		draw shape color: #black;
	}

}

species node_agent {
	float leave <- -1.0;
	rgb my_color <- #black;
	rgb my_border <- #black;
	bool neighbourhood_thinks_alike;
	bool homogeneous_network;
	string history <- "Initialized";
	bool media_proleave <- flip(perc_reach_leave_media);
	bool media_proremain <- flip(perc_reach_remain_media);

	reflex set_real_leave_value when: leave = -1.0 { // Set initial value of variable leave
		if (flip(perc_undecided_voters)) {
			leave <- 0.5;
		} else if flip(perc_leave_voters) {
			leave <- rnd(0.51, 1.0);
		} else {
			leave <- rnd(0.0, 0.49);
		}

	}
	
	// This reflex is to keep track of the changes in position in each voter 
	reflex set_first_value when: history = "Initialized" {
		if leave > undecided_range_max {
			history <- "Leave";
		} else if leave < undecided_range_min {
			history <- "Remain";
		} else {
			history <- "Undecided";
		}

	}

	reflex color_neighbors {
		float n_leave <- 0.0;
		float n_remain <- 0.0;
		list<node_agent> list_neighbors <- list<node_agent>(the_graph neighbors_of (self));
		if (empty(list_neighbors)) {
			neighbourhood_thinks_alike <- true;
			homogeneous_network <- true;
		} else {
			int nb_neighbours <- length(list_neighbors);
			float nb_neighbours_mean_leave_value <- (list_neighbors sum_of (each.leave) / nb_neighbours);
			if ((nb_neighbours_mean_leave_value > 0.5 and leave > 0.5) or (nb_neighbours_mean_leave_value < 0.5 and leave < 0.5)) {
				neighbourhood_thinks_alike <- true;
			} else {
				neighbourhood_thinks_alike <- false;
			}

			loop i over: list_neighbors {
				if (i.leave > 0.5) {
					n_leave <- n_leave + 1;
				}

				if (i.leave < 0.5) {
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



	reflex media_effect {
		// Homogenous network
		if (homogeneous_network) {
		// pro-Leave
			if (leave > 0.5) {
			// pro-Leave media has an effect, sums 0.1 to leave
				if (media_proleave and !media_proremain and leave < 1) {
					leave <- leave + media_effect_rate;
				}
				// pro-Remain media does not have an effect because the neighborhood is homogenous
				// The nodes and the neighbors think alike --> pro-Leave
				if (media_proremain and !media_proleave and leave > 0) {
					leave <- leave;
				}

			}
			// pro-Remain
			if (leave < 0.5) {
			// pro-Leave media does not have an effect
				if (media_proleave and !media_proremain and leave < 1) {
					leave <- leave;
				}
				// pro-Remain media has an effect, subtracts 0.1 to leave
				if (media_proremain and !media_proleave and leave > 0) {
					leave <- leave - media_effect_rate;
				}

			}

			// Heterogenous network
			} else if (!homogeneous_network) {

				if (media_proleave and !media_proremain and leave < 1) {
					leave <- leave + media_effect_rate;
				}

				if (media_proremain and !media_proleave and leave > 0) {
					leave <- leave - media_effect_rate;
				}

			}

			do update_color_and_history;
			}

	reflex agent_effect {
		if (!neighbourhood_thinks_alike) {
			if (leave > 0.5) {
				leave <- leave - agent_effect_rate;
			} else if (leave < 0.5) {
				leave <- leave + agent_effect_rate;
			} else { 
			/*  If the voter is exactly 0.5, undecided/neutral, 
			 it does not exactly the color of their neighbors (it only knows if they are like me or not)
			 Decide randomly between both sides, and the correct side will be selected automatically thanks to reinforcing that later */	
				leave <- rnd(0.49, 0.51); 
			}

		} else if (neighbourhood_thinks_alike) { 
			if (leave > 0.5 and leave < 1) { // Not including 0.5 because its exactly the equilibrium, remains at 0.5
				leave <- leave + agent_effect_rate;
			} else if (leave < 0.5 and leave > 0) {
				leave <- leave - agent_effect_rate;
			}

		}

		do update_color_and_history;
	}

	action update_color_and_history {
		if leave > undecided_range_max {
			my_color <- rgb(255, 255 * (1 - leave + 0.5), 255 * (1 - leave + 0.5)); // if leave = 1, color (255,0,0)
			if last(history) = 'd' { // If Undecided -> new Leave
				n_changes_to_leave <- n_changes_to_leave + 1;
				history <- history + ", Leave";
			}

		} else if leave < undecided_range_min {
			my_color <- rgb(255 * (leave + 0.5), 255 * (leave + 0.5), 255); // if leave = 0, color (0,0,255)
			if last(history) = 'd' { // If Undecided -> new Remain
				n_changes_to_remain <- n_changes_to_remain + 1;
				history <- history + ", Remain";
			}

		} else {
			my_color <- rgb(255, 255, 255); // Blanco
			if last(history) = 'n' { // If Remain -> new Undecided
				n_changes_to_undecided <- n_changes_to_undecided + 1;
				history <- history + ", Undecided";
			}

			if last(history) = 'e' { // If Leave -> new Undecided
				n_changes_to_undecided <- n_changes_to_undecided + 1;
				history <- history + ", Undecided";
			}

		} // We color de border of the node to know under which media effect it is
		if (media_proleave){
			my_border <- #red;
		}
		if (media_proremain){
			my_border <- #blue;
		}
		if (media_proleave and media_proremain){
			my_border <- rgb(155, 89, 182); // light purple
		}
		if (!media_proleave and !media_proremain){
			my_border <- #grey;
		}

	}

	aspect default {
		draw circle(1) color: my_color border: my_border;
		draw string(the_graph degree_of self) color: #black size: 4 at: {self.location.x - 1, self.location.y - 2};
	}

}

experiment leave_campaign type: gui  {
	
	// For a side-by-side model comparison
	float seedValue <- 10.0;
    float seed <- seedValue; // force the value of the seed with the values from Remain model
    // Compare Remain and Leave model
    init {
        create simulation with:[seed::seedValue,perc_leave_voters::0.3192];
    }
	
	// Save results at the end of the simulation
	reflex save_results  {
    ask simulations {
    save [self.name, [n_leave_voters, n_remain_voters]] to: "gui.csv" format:csv rewrite:false;
    // This documents can be later open in Gephi to improve the visualization of the graph
    save the_graph to:"my_graph.graphml" format:"graphml";
    save the_graph to:"my_graph.g6" format: "graph6" ;
	save the_graph to:"my_graph.gexf" format:"gexf" ;
    }
  }
	// To choose the layout in the experiment 
	user_command "Layout graph" {
	ask world {
		do layout_graph;
	}

}
	parameter "Graph type" var: graph_type;
	parameter "Number of nodes" var: nb_nodes min: 5;
	parameter "Probability to rewire an edge (beta)" var: p min: 0.0 max: 1.0 category: "small-world";
	parameter "Base degree of each node. k must be even" var: k min: 2 max: 10 category: "small-world";
	parameter "Number of edges added per novel node" var: m min: 1 max: 10 category: "scale-free";
	output {
		monitor "Leave voters" value: n_leave_voters;
		monitor "Remain voters" value: n_remain_voters;
		monitor "Neutral voters" value: n_undecided_voters;
		monitor "Total n_changes_to_leave" value: n_changes_to_leave;
		monitor "Total n_changes_to_remain" value: n_changes_to_remain;
		monitor "Total n_changes_to_undecided" value: n_changes_to_undecided;
		display map type: 2d {
			species edge_agent;
			species node_agent;
		}

		display chart refresh: every(1 #cycles) type: 2d {
			chart "Results" type: series style: spline {
				data "Leave voters" value: n_leave_voters color: #lightcoral marker: false;
				data "Remain voters" value: n_remain_voters color: #lightskyblue marker: false;
				data "Neutral voters" value: n_undecided_voters color: #grey marker: false;
			}

		}
	}

}

experiment leave type: batch repeat:1 until: (cycle = 30) {
	
	reflex save_results when: (cycle > 1){
    ask simulations {
    save [self.name, [n_leave_voters, n_remain_voters, n_undecided_voters]] to: "leave_batch_small_wordl.csv" format:csv rewrite:false;
    }
  }
	// To choose the layout in the experiment 
	user_command "Layout graph" {
	ask world {
		do layout_graph;
	}

}
	parameter "Graph type" var: graph_type;
	parameter "Number of nodes" var: nb_nodes min: 5;
	parameter "Probability to rewire an edge (beta)" var: p min: 0.0 max: 1.0 category: "small-world";
	parameter "Base degree of each node. k must be even" var: k min: 2 max: 10 category: "small-world";
	parameter "Number of edges added per novel node" var: m min: 1 max: 10 category: "scale-free";
	output {
		monitor "Leave voters" value: n_leave_voters;
		monitor "Remain voters" value: n_remain_voters;
		monitor "Neutral voters" value: n_undecided_voters;
		monitor "Total n_changes_to_leave" value: n_changes_to_leave;
		monitor "Total n_changes_to_remain" value: n_changes_to_remain;
		monitor "Total n_changes_to_undecided" value: n_changes_to_undecided;
		display map type: 2d {
			species edge_agent;
			species node_agent;
		}

		display chart refresh: every(1 #cycles) type: 2d {
			chart "Results" type: series style: spline {
				data "Leave voters" value: n_leave_voters color: #lightcoral marker: false;
				data "Remain voters" value: n_remain_voters color: #lightskyblue marker: false;
				data "Neutral voters" value: n_undecided_voters color: #grey marker: false;
			}

		}
	}

}
