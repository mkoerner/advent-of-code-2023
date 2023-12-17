// --std=c++20

#include <vector>
#include <cstdint>

#include <variant>
#include <iostream>
#include <fstream>
#include <algorithm>

template <typename F>
size_t readline(std::istream& is, F&&f, std::vector<decltype(f(char{}))>& line) {
    size_t count = 0;
    while (!is.eof()) {
        if (char c = is.get(); c == '\n') {
            return count;
        } else {
            ++count;
            line.push_back(f(c));
        }
    }
    return count;
}

struct R final {}; struct L final {}; struct D final {}; struct U final {};
using Dir = std::variant<R,L,D,U>;

struct Pos final {
    int32_t r;
    int32_t c;
    Pos left() const { return {r, c - 1}; }
    Pos right() const { return {r, c + 1}; }
    Pos up() const { return {r - 1, c}; }
    Pos down() const { return {r + 1, c}; }
};

struct Move final {
    Move(const Dir d, const Pos p) : d(d), p(p) {}
    Dir d; // direction we have come in from
    Pos p; // position we are at now
};

struct Tile final {
    char c;
    bool l = false;
    bool r = false;
    bool u = false;
    bool d = false;
};

struct Area final {
    std::vector<Tile> data;
    Pos size;
    Tile& operator[](const Pos rc) {
        return data[rc.r * size.c + rc.c];
    }
    const Tile& operator[](const Pos rc) const {
        return data[rc.r * size.c + rc.c];
    }
    bool contains(const Pos rc) const {
        return (0 <= rc.r) && (rc.r < size.r) && (0 <= rc.c) && (rc.c < size.c);
    }
    void clear() {
        for (int32_t r = 0; r < size.r; ++r) {
            for (int32_t c = 0; c < size.c; ++c) {
                (*this)[Pos{r,c}] = Tile{(*this)[Pos{r,c}].c};
            }
        }
    }
};

size_t count(const Area& area) {
    size_t count = 0;
    for (int32_t r = 0; r < area.size.r; ++r) {
        for (int32_t c = 0; c < area.size.c; ++c) {
            const Tile& tile = area[Pos{r,c}];
            if (tile.r || tile.l || tile.u || tile.d) {
                ++count;
            }
        }
    }
    return count;
}

std::istream& operator>>(std::istream& is, Area& area) {
    area.size.r = 0;
    area.size.c = 0;
    area.data.clear();
    auto transform = [](const char c) {return Tile{c}; };
    size_t read = readline(is, transform, area.data);
    area.size.c = read;
    while (read > 0 && read == area.size.c) {
        ++area.size.r;
        read = readline(is, transform, area.data);
    }
    return is;
}


std::ostream& operator<<(std::ostream& os, const Area& area) {
    for (int32_t r = 0; r < area.size.r; ++r) {
        for (int32_t c = 0; c < area.size.c; ++c) {
            const Tile& tile = area[Pos{r,c}];
            size_t count = (size_t)tile.r + (size_t)tile.l + (size_t)tile.u + (size_t)tile.d;
            if (count == 0) {
                os << tile.c;
            } else if (count == 1) {
                os << (tile.r ? '>' : tile.l ? '<' : tile.u ? '^' : 'v');
            } else {
                os << count;
            } 
        }
        os << '\n';
    }
    return os;
}

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

bool step(std::vector<Move>& moves, Area& area) {
    if (moves.empty()) {
        return false;
    }
    Move move = moves.back();
    moves.pop_back();
    Tile& tile = area[move.p];
    auto right = [&](const R) {
        if (!tile.r) {
            tile.r = true;
            if ((tile.c == '.' || tile.c == '-') && area.contains(move.p.right())) {
                moves.emplace_back(R{}, move.p.right());
            }
            if ((tile.c == '/' || tile.c == '|') && area.contains(move.p.up())) {
                moves.emplace_back(U{}, move.p.up()); 
            }
            if ((tile.c == '\\' || tile.c == '|') && area.contains(move.p.down())) {
                moves.emplace_back(D{}, move.p.down()); 
            }
        }
    };
    auto left = [&](const L) {
        if (!tile.l) {
            tile.l = true;
            if ((tile.c == '.' || tile.c == '-') && area.contains(move.p.left())) {
                moves.emplace_back(L{}, move.p.left());
            }
            if ((tile.c == '/' || tile.c == '|') && area.contains(move.p.down())) {
                moves.emplace_back(D{}, move.p.down()); 
            }
            if ((tile.c == '\\' || tile.c == '|') && area.contains(move.p.up())) {
                moves.emplace_back(U{}, move.p.up()); 
            }
        }
    };
    auto down = [&](const D) {
        if (!tile.d) {
            tile.d = true;
            if ((tile.c == '.' || tile.c == '|') && area.contains(move.p.down())) {
                moves.emplace_back(D{}, move.p.down());
            }
            if ((tile.c == '/' || tile.c == '-') && area.contains(move.p.left())) {
                moves.emplace_back(L{}, move.p.left()); 
            }
            if ((tile.c == '\\' || tile.c == '-') && area.contains(move.p.right())) {
                moves.emplace_back(R{}, move.p.right()); 
            }
        }
    };
    auto up = [&](const U) {
        if (!tile.u) {
            tile.u = true;
            if ((tile.c == '.' || tile.c == '|') && area.contains(move.p.up())) {
                moves.emplace_back(U{}, move.p.up());
            }
            if ((tile.c == '/' || tile.c == '-') && area.contains(move.p.right())) {
                moves.emplace_back(R{}, move.p.right()); 
            }
            if ((tile.c == '\\' || tile.c == '-') && area.contains(move.p.left())) {
                moves.emplace_back(L{}, move.p.left()); 
            }
        }
    };
    std::visit(overloaded{right, left, down, up}, move.d);
    return true;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    Area area;
    input >> area;
    std::cout << area << '\n';

    std::vector<Move> moves{};
    moves.emplace_back(R{}, Pos{0,0});
    while(step(moves, area)) {}

    std::cout << area << '\n';
    std::cout << count(area) << '\n';

    std::vector<size_t> scores;
    for (int32_t r = 0; r < area.size.r; ++r) {
        area.clear();
        moves.emplace_back(R{}, Pos{r,0});
        while(step(moves, area)) {}
        scores.push_back(count(area));
        area.clear();
        moves.emplace_back(L{}, Pos{area.size.r - 1,0});
        while(step(moves, area)) {}
        scores.push_back(count(area));
    }
    for (int32_t c = 0; c < area.size.c; ++c) {
        area.clear();
        moves.emplace_back(D{}, Pos{0,c});
        while(step(moves, area)) {}
        scores.push_back(count(area));
        area.clear();
        moves.emplace_back(U{}, Pos{0,area.size.c - 1});
        while(step(moves, area)) {}
        scores.push_back(count(area));
    }
    std::cout << *std::max_element(scores.begin(), scores.end()) << '\n';

    return 0;
}