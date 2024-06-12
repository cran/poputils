#include "cpp11.hpp"
#include "cpp11/doubles.hpp"
#include "cpp11/matrix.hpp"
using namespace cpp11;
using namespace std;
namespace writable = cpp11::writable;


// Helper functions -----------------------------------------------------------

double make_nx(string age) {
  if (age == "0")
    return 1.0;
  if (age == "1-4")
    return 4.0;
  if (age == "single")
    return 1.0;
  if (age == "five")
    return 5.0;
  if (age == "open")
    return R_PosInf;
  stop("Internal error: Invalid value for 'age'.");  // # nocov
}

// HAS_TESTS
[[cpp11::register]]
writable::logicals is_ax_le_nx(doubles ax,
			       strings age_group_categ) {
  int n = ax.size();
  writable::logicals ans(n);
  for (int i = 0; i < n; i++) {
    double ax_i = ax[i];
    if (isnan(ax_i))
      ans[i] = true;
    else {
      string age = age_group_categ[i];
      double nx_i = make_nx(age);
      ans[i] = (ax_i <= nx_i);
    }
  }
  return ans;
}

double make_qx_ax(double mx,
		  double ax,
		  double nx) {
  double ans = 1;
  if (isfinite(mx) && isfinite(nx)) {
    ans = (nx * mx) / (1 + (nx - ax) * mx);
    if (ans > 1)
      ans = 1;
  }
  return ans;
}


// make_ax_ij_mx --------------------------------------------------------------------

double make_ax_ij_mx_const(double mx,
			   double nx) {
  double num = 1 - (nx * mx + 1) * exp(-nx * mx);
  double den = mx * (1 - exp(-nx * mx));
  return num / den;
}

double make_ax_ij_mx_closed(double mx,
			    double nx,
			    string method) {
  if (method == "constant")
    return make_ax_ij_mx_const(mx, nx);
  else if (method == "linear")
    return 0.5 * nx;
  else
    stop("Internal error: Invalid value for 'method'."); // # nocov
}

double make_ax_ij_mx_open(double mx,
			  string method) {
  if (method == "constant")
    return 1 / mx;
  else
    stop("Internal error: Invalid value for 'method'."); // # nocov
}


double make_ax_ij_mx_infant(double m0,
			    string sex,
			    string method) {
  if (method == "CD") {
    if (sex == "Female")
      return (m0 >= 0.107) ? 0.35 : (0.053 + 2.8 * m0);
    else if (sex == "Male")
      return (m0 >= 0.107) ? 0.33 : (0.045 + 2.684 * m0);
    else {
      stop("Internal error: Invalid value for 'sex'."); // # nocov
    }
  }
  else if (method == "constant") {
    return make_ax_ij_mx_const(m0, 1.0);
  }
  else if (method == "AK") {
    if (sex == "Female") {
      if (m0 >= 0.06891)
	return 0.31411;
      else if (m0 >= 0.01724)
	return 0.04667 + 3.88089 * m0;
      else
	return 0.14903 - 2.05527 * m0;
    }
    else if (sex == "Male") {
      if (m0 >= 0.08307)
	return 0.29915;
      else if (m0 >= 0.023)
	return 0.02832 + 3.26021 * m0;
      else
	return 0.14929 - 1.99545 * m0;
    }
    else {
      stop("Internal error: Invalid value for 'sex'."); // # nocov
    }
  }
  else if (method == "linear") {
    return 0.5;
  }
  else {
    stop("Internal error: Invalid value for 'sex'."); // # nocov
  }
}    
  
double make_ax_ij_mx_child(double mx,
			   double m0,
			   string sex,
			   string method) {
  if (method == "CD") {
    if (sex == "Female")
      return (m0 >= 0.107) ? 1.361 : (1.522 - 1.518 * m0);
    else if (sex == "Male")
      return (m0 >= 0.107) ? 1.352 : (1.651 - 2.816 * m0);
    else {
      stop("Internal error: Invalid value for 'sex'."); // # nocov
    }
  }
  else if (method == "constant") {
    return make_ax_ij_mx_const(mx, 4.0);
  }
  else if (method == "linear") {
    return 2.0;
  }
  else {
    stop("Internal error: Invalid value for 'method'."); // # nocov
  }
}

double make_ax_ij_mx(double mx,
		     double m0,
		     string age,
		     string sex,
		     strings methods) {
  if (age == "0")
    return make_ax_ij_mx_infant(m0, sex, methods[0]);
  else if (age == "1-4")
    return make_ax_ij_mx_child(mx, m0, sex, methods[1]);
  else if (age == "single")
    return make_ax_ij_mx_closed(mx, 1.0, methods[2]);
  else if (age == "five")
    return make_ax_ij_mx_closed(mx, 5.0, methods[2]);
  else if (age == "open")
    return make_ax_ij_mx_open(mx, methods[3]);
  else
    stop("Internal error: Invalid value for 'method'."); // # nocov
}


// make_ax_ij_qx --------------------------------------------------------------

double make_ax_ij_qx_const(double qx,
			   double nx) {
  double num = nx * ((1 - log(1 - qx)) * (1 - qx) - 1);
  double den = log(1 - qx) * qx;
  return num / den;
}

double make_ax_ij_qx_closed(double qx,
			    double nx,
			    string method) {
  if (method == "constant")
    return make_ax_ij_qx_const(qx, nx);
  else if (method == "linear")
    return 0.5 * nx;
  else
    stop("Internal error: Invalid value for 'method'."); // # nocov
}

double make_ax_ij_qx_infant(double q0,
			    string sex,
			    string method) {
  if (method == "CD") {
    if (sex == "Female")
      return (q0 >= 0.1) ? 0.35 : (0.05 + 3 * q0);
    else if (sex == "Male")
      return (q0 >= 0.1) ? 0.33 : (0.0425 + 2.875 * q0);
    else {
      stop("Internal error: Invalid value for 'sex'."); // # nocov
    }
  }
  else if (method == "constant") {
    return make_ax_ij_qx_const(q0, 1.0);
  }
  else if (method == "AK") {
    if (sex == "Female") {
      if (q0 >= 0.0658)
	return 0.3141;
      else if (q0 >= 0.017)
	return 0.0438 + 4.1075 * q0;
      else
	return 0.149 - 2.0867 * q0;
    }
    else if (sex == "Male") {
      if (q0 >= 0.0785)
	return 0.2991;
      else if (q0 >= 0.0226)
	return 0.0244 + 3.4994 * q0;
      else
	return 0.1493 - 2.0367 * q0;
    }
    else {
      stop("Internal error: Invalid value for 'sex'."); // # nocov
    }
  }
  else if (method == "linear") {
    return 0.5;
  }
  else {
    stop("Internal error: Invalid value for 'sex'."); // # nocov
  }
}    
  
double make_ax_ij_qx_child(double qx,
			   double q0,
			   string sex,
			   string method) {
  if (method == "CD") {
    if (sex == "Female")
      return (q0 >= 0.1) ? 1.361 : (1.524 - 1.625 * q0);
    else if (sex == "Male")
      return (q0 >= 0.1) ? 1.352 : (1.653 - 3.013 * q0);
    else {
      stop("Internal error: Invalid value for 'sex'."); // # nocov
    }
  }
  else if (method == "constant") {
    return make_ax_ij_qx_const(qx, 4.0);
  }
  else if (method == "linear") {
    return 2.0;
  }
  else {
    stop("Internal error: Invalid value for 'method'."); // # nocov
  }
}

double make_ax_ij_qx(double qx,
		     double q0,
		     string age,
		     string sex,
		     strings methods) {
  if (age == "0")
    return make_ax_ij_qx_infant(q0, sex, methods[0]);
  else if (age == "1-4")
    return make_ax_ij_qx_child(qx, q0, sex, methods[1]);
  else if (age == "single")
    return make_ax_ij_qx_closed(qx, 1.0, methods[2]);
  else if (age == "five")
    return make_ax_ij_qx_closed(qx, 5.0, methods[2]);
  else if (age == "open")                                              // # nocov
    stop("Internal error: Cannot calculate 'ax' for open age group."); // # nocov
  else
    stop("Internal error: Invalid value for 'age'."); // # nocov
}





// 'Lx_to' --------------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> Lx_to_ex(cpp11::doubles_matrix<> Lx,
				    cpp11::doubles_matrix<> lx) {
  int m = Lx.nrow();
  int n = Lx.ncol();
  writable::doubles_matrix<> Tx(m, n);
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    Tx(m - 1, j) = Lx(m - 1, j);
  for (int i = m - 2; i >= 0; i--) {
    for (int j = 0; j < n; j++) {
      double Tx_end = Tx(i + 1, j);
      Tx(i, j) = Lx(i, j) + Tx_end;
    }
  }
  for (int i = 0; i < m; i++)
    for (int j = 0; j < n; j++)
      ans(i, j) = Tx(i, j) / lx(i, j);
  return ans;
}


// 'lx_to' --------------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> lx_to_dx(cpp11::doubles_matrix<> lx) {
  int m = lx.nrow();
  int n = lx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int i = 0; i < m - 1; i++) {
    for (int j = 0; j < n; j++) {
      ans(i, j) = lx(i, j) - lx(i + 1, j);
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = lx(m - 1, j);
  return ans;
}

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> lx_to_qx(cpp11::doubles_matrix<> lx) {
  int m = lx.nrow();
  int n = lx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int i = 0; i < m - 1; i++) {
    for (int j = 0; j < n; j++) {
      double px_ij = lx(i + 1, j) / lx(i, j);
      ans(i, j) = 1 - px_ij;
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = 1;
  return ans;
}


// 'mx_to' --------------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_ex(cpp11::doubles_matrix<> mx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(1, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 0.0;
  for (int j = 0; j < n; j++)
    lx[j] = 1.0;
  for (int i = 0; i < m; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans(0, j) = NA_REAL;
      }
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij_mx(mx_ij, mx(0, j), age, sex[0], methods);
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double lx_ij_start = lx[j];
	double px_ij = 1 - qx_ij;
	double lx_ij_end = px_ij * lx_ij_start;
	double dx_ij = lx_ij_start - lx_ij_end;
	double Lx_ij = dx_ij * ax_ij;
	if (isfinite(nx_i))
	  Lx_ij += lx_ij_end * nx_i;
	ans(0, j) += Lx_ij;
	lx[j] = lx_ij_end;
      }
      else {
	ans(0, j) += lx[j] * nx_i;
      }
    }
  }
  return ans;
}
	       
// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx(cpp11::doubles_matrix<> mx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1.0;
  for (int i = 0; i < m - 1 ; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij))
	ans(i + 1, j) = NA_REAL;
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij_mx(mx_ij, mx(0, j), age, sex[0], methods);
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double px_ij = 1 - qx_ij;
	double lx_ij_start = ans(i, j);
	double lx_ij_end = px_ij * lx_ij_start;
	ans(i + 1, j) = lx_ij_end;
      }
      else {
	double lx_ij_end = ans(i, j);
	ans(i + 1, j) = lx_ij_end;
      }
    }
  }
  return ans;
}

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_Lx(cpp11::doubles_matrix<> mx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1.0;
  for (int i = 0; i < m; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans(i, j) = NA_REAL;
	lx[j] = NA_REAL;
      }
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij_mx(mx_ij, mx(0, j), age, sex[0], methods);
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double px_ij = 1 - qx_ij;
	double lx_ij_start = lx[j];
	double lx_ij_end = px_ij * lx_ij_start;
	double dx_ij = lx_ij_start - lx_ij_end;
	double Lx_ij = dx_ij * ax_ij;
	if (isfinite(nx_i))
	  Lx_ij += lx_ij_end * nx_i;
	ans(i, j) = Lx_ij;
	lx[j] = lx_ij_end;
      }
      else {
	ans(i, j) = lx[j] * nx_i;
      }
    }
  }
  return ans;
}

  
// 'qx_to' --------------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> qx_to_ex(cpp11::doubles_matrix<> qx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(1, n);
  writable::doubles lx(n);
  writable::doubles Lx_prev(n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 0.0;
  for (int j = 0; j < n; j++)
    lx[j] = 1.0;
  // all age groups except final age group
  for (int i = 0; i < m - 1; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double qx_ij = qx(i, j);
      double Lx_ij; 
      if (isnan(qx_ij)) {
	Lx_ij = NA_REAL;
	lx[j] = NA_REAL;
      }
      else if (qx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij_qx(qx_ij, qx(0, j), age, sex[0], methods);
	double lx_ij_start = lx[j];
	double dx_ij = qx_ij * lx_ij_start;
	double lx_ij_end = lx_ij_start - dx_ij;
	Lx_ij = dx_ij * ax_ij + lx_ij_end * nx_i;
	lx[j] = lx_ij_end;
      }
      else {
	Lx_ij = lx[j] * nx_i;
      }
      ans(0, j) += Lx_ij;
      if (i == m - 2)
	Lx_prev[j] = Lx_ij;
    }
  }
  // final age group
  double ax_i = ax[m - 1];
  int has_ax_i = !isnan(ax_i);
  for (int j = 0; j < n; j++) {
    double lx_ij_start = lx[j];
    double Lx_ij;
    if (has_ax_i) {
      Lx_ij = lx_ij_start * ax_i;
    }
    else {
      double qx_ij_prev = qx(m - 2, j);
      if (isnan(qx_ij_prev)) {
	Lx_ij = NA_REAL;
      }
      else {
	double Lx_ij_prev = Lx_prev[j];
	if ((qx_ij_prev < 1) && (Lx_ij_prev > 0))  {
	  double dx_ij_prev = lx_ij_start * qx_ij_prev / (1 - qx_ij_prev);
	  double mx_ij_prev = dx_ij_prev / Lx_ij_prev;
	  Lx_ij = lx_ij_start / mx_ij_prev;
	}
	else {
	  Lx_ij = 0;
	}
      }
    }
    ans(0, j) += Lx_ij;
  }
  return ans;
}



// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> qx_to_lx(cpp11::doubles_matrix<> qx) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    for (int j = 0; j < n; j++) {
      double px_ij = 1 - qx(i, j);
      ans(i + 1, j) = px_ij * ans(i, j);
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> qx_to_Lx(cpp11::doubles_matrix<> qx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1.0;
  // all age groups except final age group
  for (int i = 0; i < m - 1; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double qx_ij = qx(i, j);
      if (isnan(qx_ij)) {
	ans(i, j) = NA_REAL;
	lx[j] = NA_REAL;
      }
      else if (qx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij_qx(qx_ij, qx(0, j), age, sex[0], methods);
	double lx_ij_start = lx[j];
	double dx_ij = qx_ij * lx_ij_start;
	double lx_ij_end = lx_ij_start - dx_ij;
	double Lx_ij = dx_ij * ax_ij + lx_ij_end * nx_i;
	ans(i, j) = Lx_ij;
	lx[j] = lx_ij_end;
      }
      else {
	ans(i, j) = lx[j] * nx_i;
      }
    }
  }
  // final age group
  double ax_i = ax[m - 1];
  int has_ax_i = !isnan(ax_i);
  for (int j = 0; j < n; j++) {
    double lx_start = lx[j];
    if (has_ax_i) {
      ans(m - 1, j) = lx_start * ax_i;
    }
    else {
      double qx_prev = qx(m - 2, j);
      double Lx_prev = ans(m - 2, j);
      if (isnan(qx_prev) || isnan(Lx_prev)) {
	ans(m - 1, j) = NA_REAL;
      }
      else if ((qx_prev < 1) && (Lx_prev > 0)) {
	double dx_prev = lx_start * qx_prev / (1 - qx_prev);
	double mx_prev = dx_prev / Lx_prev;
	ans(m - 1, j) = lx_start / mx_prev;
      }
      else {
	ans(m - 1, j) = 0;
      }
    }
  }
  return ans;
}



