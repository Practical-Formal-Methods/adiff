
int max(int x, int y) {
	if (x >= y) {
		return x;
	} else {
		return y;
	}
}

int min(int x, int y) {
	if (x <= y) {
		return x;
	} else {
		return y;
	}
}

int main() {
	return max(1, min(2,3));
}

