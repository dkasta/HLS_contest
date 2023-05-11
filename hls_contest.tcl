proc brave_opt args {
    array set options {-lambda 0}
    if { [llength $args] != 2 } {
        return -code error "Use brave_opt with -lambda \$latency_value\$"
    }
    foreach {opt val} $args {
        if {![info exist options($opt)]} {
        return -code error "unknown option \"$opt\""
        }
        set options($opt) $val
    }
    set latency_value $options(-lambda)

    #OUR CODE
    set mob [mobility $latency_value ]
    set res_malc [malc $latency_value]
    if { [llength $res_malc] != 2 } {
        puts "Impossible to run MLAC"
    } else {
        set res_info [lindex $res_malc 1 ]
        set res_mlac [mlac $res_info ]
        puts $res_mlac
        #puts "LEFT-EDGE"
        set colors [list]
        set can_be_optimized 0
        foreach op_pair $res_info {
            set op_name [ lindex $op_pair 0 ]
            set scheduling_per_op [list]
            foreach node $res_mlac {
                if { [ get_attribute [ lindex $node 0 ] operation ] eq $op_name } {
                    lappend scheduling_per_op $node
                }
            }
            set res_color [ left_edge $scheduling_per_op ]
            set fus_op [ get_lib_fus_from_op_delay_sorted $op_name ] 
            foreach color $res_color {
                set can_be_slower 1
				set mobility_upper_limit $latency_value
                foreach node $color {
                    set m_node [ lindex [ lindex $mob [lsearch -index 0 $mob $node] ] 1 ]
                    if { $m_node == 0 } {
                        set can_be_slower 0
                        break
                    } else {
		    			if { $mobility_upper_limit > $m_node } {
		    				set mobility_upper_limit $m_node
						}	
		    		}
                }
                if { $can_be_slower == 1 } {
                    set can_be_optimized 1
                }
                set node_op [get_attribute $node operation]
                set fu_id_fastest [ get_fastest_id [ get_lib_fus_from_op_delay_sorted $node_op ] ]
				#SLACK has to be computed as [mobility_upper_limit + delay_of_fastest_fu] !!!!!!!!!!!!!!!!!!!!
				set slack [expr { $mobility_upper_limit + [get_attribute $fu_id_fastest delay]}]
                lappend colors "{$color} $can_be_slower $node_op $fu_id_fastest $slack"
            }
            
        }
		
        if { $can_be_optimized == 0 } {
            puts "No optimization possible"
			return
		}
		set result [get_cost_mapping_allocated $res_mlac $colors]
       	set first_opt "{$res_mlac} {[lindex $result 1]} {[lindex $result 2]} [lindex $result 0]"
       	set optimal_solution [ explore_colors $colors 0 $latency_value $first_opt]
		set ret_values [list]
		lappend ret_values [lindex $optimal_solution 0]
		lappend ret_values [lindex $optimal_solution 1]
		lappend ret_values [lindex $optimal_solution 2]
		
		puts "Optimized solution"
       	foreach item $optimal_solution {
       	    puts "Print"
       	    puts $item
       	}
		return $ret_values
    }
	return
}
#//////// DATA STRUCTURES \\\\\\\\
#scheduling: < node_id, start_time >
#mapping: < node_id, fu_id >
#allocated: < fu_id, n_allocated >
#optimal_solution: < sheduling, mapping, allocated, cost >
#same_color_nodes: < node_id >
#colors: < same_color_nodes, 0/1, operation, fu_id, slack >
proc explore_colors { colors idx latency_constraint optimal_solution} {
    if { $idx < [llength $colors] } {
        set current_color [lindex $colors $idx]
        #lappend current_color "FU"
		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        set current_op [get_attribute [lindex [ lindex $current_color 0 ] 0] operation] 
		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		set current_slack [lindex $current_color end]
		#puts "current_slack=$current_slack"
        #puts "current color is $idx, with nodes [lindex $current_color 0]"
        #puts "can be changed -- [lindex $current_color 1]"
        if { [lindex $current_color 1] == 1} {
            #puts "current_color is changeable:, available FUs: [get_lib_fus_from_op $current_op]"
            foreach fu [get_lib_fus_from_op $current_op] {
				if { [get_attribute $fu delay] <= $current_slack } {
                	set current_color [lreplace $current_color end-1 end-1 $fu]
                	set colors [lreplace $colors $idx $idx $current_color]
                	set optimal_solution [explore_colors $colors [expr {$idx + 1}] $latency_constraint $optimal_solution]
				}
            }
        } else {
            #puts "This color is not changeable, leaving the fastest resource"
            # set id_fu [ get_fastest_id [ get_lib_fus_from_op_delay_sorted $current_op ] ]
            # set current_color [lreplace $current_color end-1 end-1 $id_fu]
            # set colors [lreplace $colors $idx $idx $current_color]
            set optimal_solution [explore_colors $colors [expr {$idx + 1}] $latency_constraint $optimal_solution]
        }
    } else {
        puts "**Solution**"
        foreach item $colors {
            puts $item
        }
        set adhoc [mlac_adhoc $colors]
        set new_scheduling [lindex $adhoc 1]
        set new_latency [lindex $adhoc 0]
        puts "$new_latency <= $latency_constraint"
        if { $new_latency <= $latency_constraint } {
            set result [get_cost_mapping_allocated $new_scheduling $colors]
            set cost [ lindex $result 0 ]
            puts "$cost < [ lindex $optimal_solution end ]"
            if { $cost < [ lindex $optimal_solution end ] } {
                set optimal_solution "{$new_scheduling} {[lindex $result 1]} {[lindex $result 2]} $cost"
            }
        }
        #puts $adhoc
        #puts "[lindex $adhoc 1]"
        # foreach item [lindex $adhoc 1] {
        #     puts "[lindex $item 0] [get_attribute [lindex $item 0] label] [lindex $item 1]"
        # }
    }
    return $optimal_solution
}

proc mobility { latency_constraint } {
    set asap_res [lsort -dictionary [lindex [asap] 1]]
    set alap_res [lsort -dictionary [alap $latency_constraint]]
    set mobility [list]
    for { set i 0 } { $i < [llength $asap_res] } { incr i } {
        set node [lindex [lindex $asap_res $i] 0]
        set start_asap [lindex [lindex $asap_res $i] 1]
        set start_alap [lindex [lindex $alap_res $i] 1]
        lappend mobility "$node [ expr { $start_alap - $start_asap } ]"
    }
    return $mobility
}

proc get_lib_fus_from_op_delay_sorted { op } {
    set fus [ get_lib_fus_from_op $op ]
    set param_fus [list]
    foreach fu $fus {
        set name [get_attribute $fu operation]
        set delay [get_attribute $fu delay]
        set area [get_attribute $fu area]
        set power [get_attribute $fu power]
        lappend param_fus "$name $area $delay $power $fu"
    }
    set param_fus [lsort -integer -index 2 $param_fus]
    return $param_fus
}

proc get_fastest_delay { fdo } {
    set delay [lindex [lindex $fdo 0 ] 2 ]
    return $delay
}

proc get_fastest_id { fdo } {
    set id [lindex [lindex $fdo 0 ] 4 ]
    return $id
}

proc alap { lambda } {

  set node_start_time [list]

  foreach node [lreverse [get_sorted_nodes]] {
    set end_time $lambda
    set node_op [get_attribute $node operation]
    set node_delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted $node_op ] ]
    foreach child [get_attribute $node children] {
        set idx_child_start [lsearch -index 0 $node_start_time $child]
        set child_start_time [lindex [lindex $node_start_time $idx_child_start] 1]
        if { $child_start_time < $end_time } {
            set end_time $child_start_time
        }
    }
    lappend node_start_time "$node [expr {$end_time - $node_delay}]"
  }

  return [lreverse $node_start_time]
}

proc asap {} {

  set node_start_time [list]
  foreach node [get_sorted_nodes] {
    set start_time 1
    foreach parent [get_attribute $node parents] {
      set parent_op [get_attribute $parent operation]
      set parent_delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted $parent_op ] ]
      set idx_parent_start [lsearch -index 0 $node_start_time $parent]
      set parent_start_time [lindex [lindex $node_start_time $idx_parent_start] 1]
      set parent_end_time [expr $parent_start_time + $parent_delay]
      if { $parent_end_time > $start_time } {
        set start_time $parent_end_time
      }
    }
    set node_op [get_attribute $node operation]
    set node_delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted $node_op ] ]
    set node_end_time [ expr {$start_time + $node_delay} ]
    lappend node_start_time "$node $start_time $node_end_time"
  }
  set latency_asap [ lindex [lindex [ lsort -decreasing -integer -index 2 $node_start_time ] 0 ] 2 ]
  set sched_start_time [list]
  foreach var $node_start_time {
      lappend sched_start_time "[lindex $var 0] [lindex $var 1]"
  }
  set ret_values [list]
  lappend ret_values $latency_asap
  lappend ret_values $sched_start_time
  return $ret_values
}

proc malc { latency } {
    
    set alap_res [alap $latency]
    #Start time of N0 must be al least 1
    set start_time_n0 [lindex [lindex $alap_res [lsearch -index 0 $alap_res "N0" ] ] 1] 
    if { $start_time_n0 < 1 } {
        puts "Impossible to run"
        return [list]
    }
    set l 1
    set node [get_nodes]
    set s_node [list]
    set res_info [list]
    set list_ret_schedule [list]
    foreach var $node {
        set op_var [get_attribute $var operation]
        if { [lsearch -index 0 $res_info $op_var ] == -1 } {
            lappend res_info "$op_var 0"
        }
    }
    while { [llength $node] != 0 } {
        foreach operation $res_info {
            set ready_list [list]
            set op_name [lindex $operation 0]
            set mult_op [lindex $operation 1]
            set count 0
            #check scheduled node
            foreach choose $s_node {
                set final_time [lindex [split $choose " "] 3 ]
                set final_op [lindex [split $choose " "] 1 ]
                set name_rem [lindex [split $choose " "] 0 ]
                if { $l == $final_time } {
                    set idx_remove [lsearch $node $name_rem ]
                    set node [lreplace $node $idx_remove $idx_remove ]
                } elseif { $l < $final_time && $final_op eq $op_name} {
                    incr count
                }
            }
            #search ready nodes
            foreach n $node {
                set name_op [get_attribute $n operation]
                if { $name_op eq $op_name } {
                    #Searching parents of working node
                    set parents [ get_attribute $n parents ]
                    set num_parents [ llength $parents ]
                    #check if all these nodes are already scheduled
                    set check 0
                    foreach par $parents {
                        if { [lsearch $node $par] == -1 } {
                            incr check
                        }
                    }
                    #if entry point or all parents scheduled
                    if { $num_parents == 0 || $check == $num_parents } {
                        set T_l [lindex [lindex $alap_res [lsearch -index 0 $alap_res $n] ] 1 ]
                        set slack [ expr { $T_l - $l} ]
                        lappend ready_list "$n $slack"
                    }
                }
            }
            set count_zero 0
            foreach to_schedule $ready_list {
                set name_of_node [lindex $to_schedule 0]
                if { [lindex $to_schedule 1] == 0 } {
                    #set node_delay [get_attribute [get_lib_fu_from_op [get_attribute $name_of_node operation] ] delay]
                    set node_delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted [get_attribute $name_of_node operation] ] ]
                    set end_time [ expr {$l + $node_delay}]
                    lappend s_node "$name_of_node $op_name $l $end_time"
                    lappend list_ret_schedule "$name_of_node $l"
                    incr count_zero
                }
            }
            set new_min_res [ expr {$count + $count_zero }] 
            if { $new_min_res > $mult_op } {
                set idx_update [lsearch -index 0 $res_info $op_name ]
                set res_info [lreplace $res_info $idx_update $idx_update "$op_name $new_min_res" ]
            }
        }
        incr l
    }
    set list_return [list]
    lappend list_return $list_ret_schedule
    lappend list_return $res_info
    
    return $list_return
}

proc mlac { res_info } {
    set l 1
    set node [get_nodes]
    set scheduled_nodes [list]
    set s_node [list]
    while { [llength $node] != 0 } {
        foreach operation $res_info {
            set ready_list [list]
            set op_name [lindex $operation 0]
            set mult_op [lindex $operation 1]
            set count 0
            foreach choose $s_node {
                set final_time [lindex [split $choose " "] 3 ]
                set final_op [lindex [split $choose " "] 1 ]
                set name_rem [lindex [split $choose " "] 0 ]
                if { $l == $final_time } {
                    set idx_remove [lsearch $node $name_rem ]
                    set node [lreplace $node $idx_remove $idx_remove ]
                } elseif { $l < $final_time && $final_op eq $op_name} {
                    incr count
                }
            }
            foreach n $node {
                set name_op [get_attribute $n operation]
                if { $name_op eq $op_name } {
                    #Searching parents of working node
                    set parents [ get_attribute $n parents ]
                    set num_parents [ llength $parents ]
                    #check if all these nodes are already scheduled
                    set check 0
                    foreach par $parents {
                        if { [lsearch $node $par] == -1 } {
                            incr check
                        }
                    }
                    #if entry point or all parents scheduled
                    if { $num_parents == 0 || $check == $num_parents } {
                        lappend ready_list $n
                    }
                }
            }
            set num_to_schedule [ expr {$mult_op - $count } ]
            for {set i 0} { $i < $num_to_schedule && $i < [llength $ready_list]} { incr i } {
                set node_to_schedule [lindex $ready_list $i]
                #set node_delay [get_attribute [get_lib_fu_from_op [get_attribute $node_to_schedule operation] ] delay]
                set node_delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted [get_attribute $node_to_schedule operation] ] ]
                set end_time [ expr {$l + $node_delay}]
                lappend s_node "$node_to_schedule $op_name $l $end_time"
                lappend scheduled_nodes "$node_to_schedule $l"
            } 
        }
        incr l
    }
    return $scheduled_nodes
}

proc busy_op_per_ccy { OP pairs } {
	set l_max [lindex [lindex [lsort -index 1 -increasing -integer $pairs] end] 1]
	set ops_per_ccy [list]
	set l 1
	while { $l <= $l_max } {
		foreach node_idx [lsearch -all -index 1 $pairs $l] {
			set node [lindex [lindex $pairs $node_idx] 0]
			set node_op [get_attribute $node operation]
            set node_delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted $node_op ] ]
    		#set node_delay [get_attribute [get_lib_fu_from_op $node_op] delay] 
			for { set i $l } { $i < [expr {$node_delay + $l}] } { incr i } { 
				if { [llength $ops_per_ccy] < $i } {
					lappend ops_per_ccy [list]
				}
				lset ops_per_ccy $i end+1 $node_op
			}
		}
		incr l
	}
	set ops_per_ccy [lreplace $ops_per_ccy 0 0]
	puts $ops_per_ccy

	set opCount_per_ccy [list]
	foreach ops $ops_per_ccy {
		lappend opCount_per_ccy "[llength [lsearch -all $ops $OP]]"
	}
	return $opCount_per_ccy
}

proc left_edge { pairs } {
    set ordered_pairs [ lsort -integer -index 1 $pairs ]
    set c 1
    set colored_nodes [list]
    while { [llength $ordered_pairs] != 0 } {
        set S [list]
        set r 0
        foreach pair $ordered_pairs {
            set node [lindex $pair 0]
            set st [lindex $pair 1]
            if { $st >= $r } {
                lappend S $node
                set delay [get_fastest_delay [ get_lib_fus_from_op_delay_sorted [get_attribute $node operation] ] ]
                #set delay [ get_attribute [ get_lib_fu_from_op [get_attribute $node operation] ] delay ]
                set r [ expr { $st + $delay } ]
                set idx_node [ lsearch -index 0 $ordered_pairs $node ]
                set ordered_pairs [lreplace $ordered_pairs $idx_node $idx_node ]
            }  
        }
        incr c
        lappend colored_nodes $S
    }
    return $colored_nodes
}

proc mlac_adhoc { colors } {
    set l 1
    set latency 0
    set node [get_nodes]
    set scheduled_nodes [list]
    set s_node [list]
    while { [llength $node] != 0 } { 
        #puts "Step: $l"
        foreach color $colors {
            set num_color [lsearch $colors $color]
            set node_delay [ get_attribute [ lindex $color end-1 ] delay ]
            set busy 1
            set idx_busy_node [lsearch -index 0 $s_node $num_color ] 
            #puts "**Colore $num_color"
            #puts "  --$s_node"
            if { $idx_busy_node == -1 } {
                set busy 0
            }
            # else {
            #     set final_time [lindex [ lindex $s_node $idx_busy_node ] 2 ]

            #     if { $final_time > $latency } { set latency $final_time }
            #     if { $l == $final_time } { 
            #         set busy 0 
            #         set name_rem [ lindex [ lindex $s_node $idx_busy_node ] 1 ]
            #         puts "......$name_rem removed"
            #         set idx_remove [lsearch $node $name_rem ]
            #         set node [lreplace $node $idx_remove $idx_remove ]
            #         set s_node [lreplace $s_node $idx_busy_node $idx_busy_node ]
            #     }
            # }
            foreach s $s_node {
                set final_time [lindex $s 2 ]
                if { $final_time > $latency } { set latency $final_time }
                if { $l == $final_time } { 
                    if { [lindex $s 0] == $num_color } {
                      set busy 0
                    } 
                    set name_rem [ lindex $s 1 ]
                    #puts "......$name_rem removed"
                    set idx_remove [lsearch $node $name_rem ]
                    set node [lreplace $node $idx_remove $idx_remove ]
                    set idx_remove_s [lsearch -index 1 $s_node $name_rem ]
                    set s_node [lreplace $s_node $idx_remove_s $idx_remove_s ]
                }
            }
            #puts "  --$s_node"

            if { $busy == 0 } {
                set ready_list [list]
                foreach n [ lindex $color 0 ] {
                    if { [lsearch $node $n] != -1 } {
                        #Searching parents of working node
                        set parents [ get_attribute $n parents ]
                        #if {$l == 4} { puts "$n -- $parents" }
                        set num_parents [ llength $parents ]
                        #check if all these nodes are already scheduled
                        set check 0
                        foreach par $parents {
                            if { [lsearch $node $par] == -1 } {
                                #puts "  $par not found"
                                incr check
                            }
                        }
                        #if entry point or all parents scheduled
                        if { $num_parents == 0 || $check == $num_parents } {
                            lappend ready_list $n
                        }
                    }   
                }
                if { [llength $ready_list] != 0 } {
                    
                    set node_to_schedule [ get_topological_first $ready_list ]
                    #puts "Step: $l $ready_list -> $node_to_schedule"
                    #set node_to_schedule [lindex $ready_list 0]
                    set end_time [ expr { $l + $node_delay } ]
                    lappend s_node "$num_color $node_to_schedule $end_time" 
                    #puts "Scheduled node $node_to_schedule"
                    lappend scheduled_nodes "$node_to_schedule $l"
                }
            }
        }
        incr l
    }
    return "$latency {$scheduled_nodes}"
}

proc get_topological_first { ready_list } {
    set ordered_nodes [get_sorted_nodes]
    #set idx_min [llength $ordered_nodes]
    set idx_min 0
    foreach nod $ready_list {
        set idx_node [lsearch $ordered_nodes $nod]
        # if { $idx_node < $idx_min } {
        #     set idx_min $idx_node
        # }
        if { $idx_node > $idx_min } {
            set idx_min $idx_node
        }
    }
    set returned_node [lindex $ordered_nodes $idx_min]
    return $returned_node
}

proc get_area { allocated_pairs } {
	set total_area 0
	foreach pair $allocated_pairs {
		set node_area [get_attribute [lindex $pair 0] area]
		set total_area [expr {$total_area + ($node_area * [lindex $pair 1])}]
	}
	return $total_area
}

proc get_power { scheduling_pairs mapping_pairs } {
	set total_power 0
	foreach node $scheduling_pairs {
		set node_id [lindex $node 0]
		set mapped_idx [lsearch -index 0 $mapping_pairs $node_id]
		#if { $mapped_idx == -1} { continue }
		set mapped_resource [lindex [lindex $mapping_pairs $mapped_idx] 1]
        #puts $mapped_resource
		set node_power [get_attribute $mapped_resource power]
		#puts "node_id $node_id || mapped_idx $mapped_idx || mapped_resource $mapped_resource || node_power $node_power"
		set total_power [expr {$total_power + $node_power}]
	}
	return $total_power
}

proc get_cost_mapping_allocated { scheduling colors } {
    set mapping [generate_mapping $colors]
    #puts "in cost M -- $mapping"
    set allocated [generate_allocated $colors]
    #puts "in cost A -- $allocated"
    set power [get_power $scheduling $mapping]
    set area [get_area $allocated]
    #set cost [expr {tcl::mathfunc::sqrt($area*$area + $power*$power) }]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    set cost [expr {$area*$area + $power*$power }]
    return "$cost {$mapping} {$allocated}"
}

proc generate_mapping { colors } {
    set mapping [list]
    foreach color $colors {
        set fu_id [ lindex $color end-1 ]
        foreach node [ lindex $color 0 ] {
            lappend mapping "$node $fu_id"
        }
    }
    return $mapping
}

proc generate_allocated { colors } {
    set allocated [list]
    foreach color $colors {
        set idx_fu [lsearch -index 0 $allocated [lindex $color end-1] ] 
        if {$idx_fu == -1} {
            lappend allocated "[lindex $color end-1] 1"
        } else {
            set fu [lindex $allocated $idx_fu]
            set count [ lindex $fu 1 ]
            #puts "[lindex $fu 0] $count"
            set fu "[lindex $fu 0] [ incr count ]"
            set allocated [ lreplace $allocated $idx_fu $idx_fu $fu ]
        }
    }
    return $allocated
}

