#include "pull_velocity.h"
//#include <stdlib.h>   //already included in common.h

static vec_t efp_pull_velocity(const struct state *state, size_t frag_i, size_t frag_j, int dt){
	struct md *md;
	vec_t pull_direction;
	vec_t pull_direction_normalized;
	vec_t pull_velocity;
	double pull_normalized;
	int time1 = dt;
	double f_const = cfg_get_double(state->cfg, "smd_force_constant");
	double v_const = cfg_get_double(state->cfg, "smd_velocity_constant");
	vec_t cv; 

	//Initial COM from common.h
	struct frag *fr_i = state->sys->frags + frag_i;
	struct frag *fr_j = state->sys->frags + frag_j;

	vec_t pull_velocity_initial_com;
	pull_velocity_initial_com.x = fr_i->coord[0];
	pull_velocity_initial_com.y = fr_i->coord[1];
	pull_velocity_initial_com.z = fr_i->coord[2];

	vec_t pull_ref_com;
	pull_ref_com.x = fr_j->coord[0];
	pull_ref_com.y = fr_j->coord[1];
	pull_ref_com.z = fr_j->coord[2];

	//Dynamic COM from md.h
	struct body *pull_frag = md->bodies + frag_i;

	vec_t pull_curr_com;
	pull_curr_com.x = pull_frag->pos.x;
	pull_curr_com.y = pull_frag->pos.y;
	pull_curr_com.z = pull_frag->pos.z;

	if(cfg_get_int(state->cfg, "pull_force_direction") == 0) {
		vec_t pull_direction = {
			pull_direction.x = fr_i->coord[0] -1,
			pull_direction.y = fr_i->coord[1] -0,
			pull_direction.z = fr_i->coord[2] -0
		};
		double pull_normalized = sqrt(pow(pull_direction.x,2)+pow(pull_direction.y,2)+pow(pull_direction.z,2));

        vec_t pull_direction_normalized = {
            pull_direction_normalized.x = pull_direction.x/pull_normalized,
            pull_direction_normalized.y = pull_direction.y/pull_normalized,
            pull_direction_normalized.z = pull_direction.z/pull_normalized
        };

        vec_t pull_velocity = {
        	pull_velocity.x = f_const*(pow(v_const*time1-(pull_curr_com.x - pull_velocity_initial_com.x)*pull_direction_normalized.x,2))/2,
        	pull_velocity.y = f_const*(pow(v_const*time1-(pull_curr_com.y - pull_velocity_initial_com.y)*pull_direction_normalized.y,2))/2,
        	pull_velocity.z = f_const*(pow(v_const*time1-(pull_curr_com.z - pull_velocity_initial_com.z)*pull_direction_normalized.z,2))/2,

        };
        vec_t cv = {pull_velocity.x, pull_velocity.y, pull_velocity.z};
        return cv;

	}

	if(cfg_get_int(state->cfg, "pull_force_direction") == 1) {
		vec_t pull_direction = {
			pull_direction.x = fr_i->coord[0] -0,
			pull_direction.y = fr_i->coord[1] -1,
			pull_direction.z = fr_i->coord[2] -0
		};
		double pull_normalized = sqrt(pow(pull_direction.x,2)+pow(pull_direction.y,2)+pow(pull_direction.z,2));

        vec_t pull_direction_normalized = {
            pull_direction_normalized.x = pull_direction.x/pull_normalized,
            pull_direction_normalized.y = pull_direction.y/pull_normalized,
            pull_direction_normalized.z = pull_direction.z/pull_normalized
        };

        vec_t pull_velocity = {
        	pull_velocity.x = f_const*(pow(v_const*time1-(pull_curr_com.x - pull_velocity_initial_com.x)*pull_direction_normalized.x,2))/2,
        	pull_velocity.y = f_const*(pow(v_const*time1-(pull_curr_com.y - pull_velocity_initial_com.y)*pull_direction_normalized.y,2))/2,
        	pull_velocity.z = f_const*(pow(v_const*time1-(pull_curr_com.z - pull_velocity_initial_com.z)*pull_direction_normalized.z,2))/2,

        };
        vec_t cv = {pull_velocity.x, pull_velocity.y, pull_velocity.z};
        return cv;

	}
	if(cfg_get_int(state->cfg, "pull_force_direction") == 2) {
		vec_t pull_direction = {
			pull_direction.x = fr_i->coord[0] -0,
			pull_direction.y = fr_i->coord[1] -0,
			pull_direction.z = fr_i->coord[2] -1
		};
		double pull_normalized = sqrt(pow(pull_direction.x,2)+pow(pull_direction.y,2)+pow(pull_direction.z,2));

        vec_t pull_direction_normalized = {
            pull_direction_normalized.x = pull_direction.x/pull_normalized,
            pull_direction_normalized.y = pull_direction.y/pull_normalized,
            pull_direction_normalized.z = pull_direction.z/pull_normalized
        };

        vec_t pull_velocity = {
        	pull_velocity.x = f_const*(pow(v_const*time1-(pull_curr_com.x - pull_velocity_initial_com.x)*pull_direction_normalized.x,2))/2,
        	pull_velocity.y = f_const*(pow(v_const*time1-(pull_curr_com.y - pull_velocity_initial_com.y)*pull_direction_normalized.y,2))/2,
        	pull_velocity.z = f_const*(pow(v_const*time1-(pull_curr_com.z - pull_velocity_initial_com.z)*pull_direction_normalized.z,2))/2,

        };
        vec_t cv = {pull_velocity.x, pull_velocity.y, pull_velocity.z};
        return cv;

	}
	if(cfg_get_int(state->cfg, "pull_force_direction") == 3) {
		vec_t pull_direction = {
			pull_direction.x = fr_i->coord[0]-fr_j->coord[0],
			pull_direction.y = fr_i->coord[1]-fr_j->coord[1],
			pull_direction.z = fr_i->coord[2]-fr_j->coord[2]
		};
		double pull_normalized = sqrt(pow(pull_direction.x,2)+pow(pull_direction.y,2)+pow(pull_direction.z,2));

        vec_t pull_direction_normalized = {
            pull_direction_normalized.x = pull_direction.x/pull_normalized,
            pull_direction_normalized.y = pull_direction.y/pull_normalized,
            pull_direction_normalized.z = pull_direction.z/pull_normalized
        };

        vec_t pull_velocity = {
        	pull_velocity.x = f_const*(pow(v_const*time1-(pull_curr_com.x - pull_velocity_initial_com.x)*pull_direction_normalized.x,2))/2,
        	pull_velocity.y = f_const*(pow(v_const*time1-(pull_curr_com.y - pull_velocity_initial_com.y)*pull_direction_normalized.y,2))/2,
        	pull_velocity.z = f_const*(pow(v_const*time1-(pull_curr_com.z - pull_velocity_initial_com.z)*pull_direction_normalized.z,2))/2,

        };
        vec_t cv = {pull_velocity.x, pull_velocity.y, pull_velocity.z};
        return cv;

	}

}
