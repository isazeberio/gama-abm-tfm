/**
* Name: prueba
* Based on the internal empty template. 
* Author: isabelazeberio
* Tags: MAS AVANZADO QUE PRUEBA
* TESTEANDO IMPROVEMENTS PARA VER QUE FUNCIONEN COMO DEBERIAN
*/
model testing

global {
	graph the_graph;
	string graph_type <- "small-world" among: ["small-world", "scale-free", "complete", "random"];
	string the_layout parameter: true init: "Circle" among: ["Circle", "Forced", "Grid"];
	int nb_nodes <- 10;
	int nb_edges <- 15;
	float p <- 0.2;
	int k <- 4;
	int m <- 4;

	// Voter type distribution
	// Calculating agent percentages 
	float pct_leave <- 0.5;
	float pct_undecided <- 0.2;
	float pct_remain <- 0.3;

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

	// IMPORTANT: define homogeneity neighborhood composition threshold 
	float homogeneous_limit <- 0.75;

	// IMPORTANT: define similarity of opinion with the neigbours thresold
	float similarity_limit <- 0.50;

	// IMPORTANT: agent and media reflex influx rate
	float media_effect_rate <- 0.02;
	float agent_effect_rate <- 0.01;

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
	// Voting preference
	float vote <- float(0.00, 1.00) min: 0.00 max: 1.00 with_precision 3;
	rgb agent_color <- #black;
	string voter_type;

	// Article information
	string article_type;
	rgb agent_border <- #black;
	string agent_readership;

	// Network composition 
	bool neighbours_think_alike;
	bool homogeneous_network;
	
	// Variable to store the dominant position in a neighbourhood
	string dominant_position;

	// Assigning attributes to each agent: Voting preference
	reflex set_vote when: time = 0 #day {
		if (int(name) < leave_index) {
			vote <- rnd(0.56, 1.00) with_precision 3;
			voter_type <- "leave";
			// Pro-leave color (255,0,0) The red component is set to a constant value of 255, maximum intensity
			// gradiant from red to white	
			agent_color <- rgb(255, 255 * (1 - (vote - 0.5) * 2), 255 * (1 - (vote - 0.5) * 2));
		} else if ((leave_index <= int(name)) and (int(name) < undecided_index)) {
			vote <- rnd(0.45, 0.55) with_precision 3;
			voter_type <- "undecided";
			// white
			agent_color <- rgb(255, 255, 255);
		} else {
			vote <- rnd(0.00, 0.44) with_precision 3;
			voter_type <- "remain";
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

	// Reflex to determine if agents think like their neighbors based on vote intention
	reflex vote_neighbours when: time != 0 #cycle {
		// Initialize variables to count the number of neighbors with different voting intentions
		float n_leave <- 0.0;
		float n_remain <- 0.0;
		float n_undecided <- 0.0;
		

		// Get the list of neighbours for the current agent
		list<node_agent> neighbours <- list<node_agent>(the_graph neighbors_of (self));
		write "neighbours of: " + self;
		write neighbours;
		write voter_type;

		// Loop through each neighbour to extract their voting type
		// Count the number of neighbours with each voting intention
		loop i over: neighbours {
			write "my neighbours is " + i.name + " and is " + i.voter_type;
			if (i.voter_type = "leave") {
				n_leave <- n_leave + 1;
			} else if (i.voter_type = "remain") {
				n_remain <- n_remain + 1;
			} else if (i.voter_type = "undecided") {
				n_undecided <- n_undecided + 1;
			} else {
				write "error: " + i.voter_type;
			} }

		write "n_leave: " + n_leave;
		write "n_remain: " + n_remain;
		write "n_undecided: " + n_undecided;

		// Calculate relative percentages of voting intentions among neighbours
		// What we want to know with this is the percentage of neighbours of a node that think like the node
		// If the percetage is bigger or equal than the similatiry likimt, then the node thinks like its neighbours
		// If it's smaller they think differently 
		float denominator <- n_remain + n_leave + n_undecided;
		if (denominator != 0) {
			float relative_pct_remain <- (n_remain / denominator);
    		float relative_pct_leave <- (n_leave / denominator);
   			float relative_pct_undecided <- (n_undecided / denominator);
   			
   			// Determine dominant position of the neighbourhood
   			if (relative_pct_remain >= relative_pct_leave and relative_pct_remain >= relative_pct_undecided){
   				 dominant_position <- "remain";
   			}
   			else if (relative_pct_leave >= relative_pct_remain and relative_pct_leave >= relative_pct_undecided){
   				dominant_position <- "leave";
   			}
   			else {
   				dominant_position <- "undecided";
   			}
   			
   			// Conditionals that allow each agent to know whether it thinks like its neighbours and whether it's in a homogenous neighbourhood
			if (voter_type = "remain") {
				// write "relative remain percentage: " + relative_pct_remain;
				if (relative_pct_remain > similarity_limit) {
					neighbours_think_alike <- true;
					// write "" + self + " thinks like its neighbours";
				} else {
					neighbours_think_alike <- false;
					// write "" + self + " does not thinks like its neighbours";
				}

				if ((n_leave / denominator) >= homogeneous_limit or 
					(n_remain / denominator) >= homogeneous_limit or 
					(n_undecided / denominator) >= homogeneous_limit){
					homogeneous_network <- true;
					// write "" + self + " is in a homogenous neighbourhood dominated by " + dominant_position;
							
				}
				else {
					homogeneous_network <- false;
					// write "" + self + " is in a heterogenous neighbourhood";
				}

			} else if (voter_type = "leave") {
				// write "relative leave percentage: " + relative_pct_leave;
				if (relative_pct_leave > similarity_limit) {
					neighbours_think_alike <- true;
					// write "" + self + " thinks like its neighbours";
				} else {
					neighbours_think_alike <- false;
					// write "" + self + " does not thinks like its neighbours";
				}
				
				if ((n_leave / denominator) >= homogeneous_limit or 
					(n_remain / denominator) >= homogeneous_limit or 
					(n_undecided / denominator) >= homogeneous_limit){
					homogeneous_network <- true;
					// write "" + self + " is in a homogenous neighbourhood dominated by " + dominant_position;
							
				}
				else {
					homogeneous_network <- false;
					// write "" + self + " is in a heterogenous neighbourhood";
					}
				

			} else if (voter_type = "undecided") {
				// write "relative undecided percentage: " + relative_pct_undecided;
				if (relative_pct_undecided > similarity_limit) {
					neighbours_think_alike <- true;
					// write "" + self + " thinks like its neighbours";
				} else {
					neighbours_think_alike <- false;
					// write "" + self + " does not thinks like its neighbours";
				}
				
				if ((n_leave / denominator) >= homogeneous_limit or 
					(n_remain / denominator) >= homogeneous_limit or 
					(n_undecided / denominator) >= homogeneous_limit){
					homogeneous_network <- true;
					// write "" + self + " is in a homogenous neighbourhood dominated by " + dominant_position;
							
				}
				else {
					homogeneous_network <- false;
					// write "" + self + " is in a heterogenous neighbourhood";
					}

			} } }
			
			
		// Media effect based on Filter hypothesis by Katz & Lazarsfeld
		// In homogenous networks:
		//		a) Congruent message => Opinion reinforcement: Strong influence
		// 		b) Conflicting message => Blockade: Weak influence
		// In heterogenous networks:
		// 		a) Open flank => Breakage: Medium influence
		reflex media {
			if(agent_readership = "reads"){
				if (homogeneous_network = true ){
					// Congruent message in a homogenous network => Opinion reinforcement: Strong influence
					if (dominant_position = "remain" and article_type = "pro-remain"){
						vote <- vote - media_effect_rate;
					}
					else if (dominant_position = "leave" and article_type = "pro-leave" ){
						vote <- vote + media_effect_rate;
					}
					
					// Conflicting message in a homogeneous => Blockade: Weak influence
					else if (dominant_position = "remain" and article_type = "pro-leave"){
						vote <- vote + ((media_effect_rate/2)/2); 
					}
					else if (dominant_position = "leave" and article_type = "pro-remain"){
						vote <- vote - ((media_effect_rate/2)/2); 
					}		
					
				}
					// In heterogenous networks Open flank => Breakage: Medium influence
				else if (homogeneous_network = false){
					if (article_type = "pro-remain") {
						vote <- vote - (media_effect_rate/2);
					}
					else if (article_type = "pro-leave") {
						vote <- vote + (media_effect_rate/2);
					}
					
				}
			}
			
			else {
				// No media effect because the agent does not read any kind of newspaper
			}
			
			
			do update;
		}

			
		reflex group_preassure {
			// if the agent think like its neighbours: Reinforcement of its prior opinion 
			if (neighbours_think_alike = true) {
				if (dominant_position = "remain") {
					vote <- vote - agent_effect_rate;
				}
				else if (dominant_position = "leave") {
					vote <- vote + agent_effect_rate;
				}
				// Dominant opinion undecided
				else {
					if (vote < 0.45) { // If agent's opinion is closer to remain
						vote <- vote + agent_effect_rate; // Move towards center
					} else if (vote > 0.45) { // If agent's opinion is closer to leave
						vote <- vote - agent_effect_rate; // Move towards center
					} // If the agent's opinion is exactly at the center, no change needed
				
				}
			}
			// If the agent does not think like its neighbours: Weakening of its prior opinion
			else if (neighbours_think_alike = false){
				if (dominant_position = "remain") {
					vote <- vote - agent_effect_rate;
				}
				else if (dominant_position = "leave"){
					vote <- vote + agent_effect_rate;
				}
				
				else {
					// If the agent is pro-leave the undecided group pressure will substract points to vote
					if (vote > 0.55) {
						vote <- vote - agent_effect_rate;
					}
					// If the agent is pro-leave the undecided group pressure will add points to vote
					else if (vote < 0.55){
						vote <- vote + agent_effect_rate;
					}
				}
			}
			do update;
		}

		action update {
			if vote > 0.55 with_precision 3 {
				agent_color <- rgb(255, 255 * (1 - (vote - 0.5) * 2), 255 * (1 - (vote - 0.5) * 2));
				voter_type <- "leave";
			} else if vote < 0.45 with_precision 3 {
				agent_color <- rgb(255 * vote * 2, 255 * vote * 2, 255);
				voter_type <- "remain";
			} else {
				agent_color <- rgb(255, 255, 255);
				voter_type <- "undecided";
			}
	
		}
		
	aspect default {
		draw circle(1) color: agent_color border: agent_border;
		draw string(the_graph degree_of self) color: #black size: 4 at: {self.location.x - 1, self.location.y - 2};
	} }

experiment loadgraph type: gui {
	
	// To choose the layout in the experiment 
	user_command "Layout graph" {
		ask world {
			do layout_graph;
		}

	}
	
	output {
		display map type: 2d {
			species edge_agent aspect: default;
			species node_agent aspect: default;
		}

	}

}