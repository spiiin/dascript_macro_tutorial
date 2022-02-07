int main( int, char * [] ) {
    NEED_ALL_DEFAULT_MODULES;
    Module::Initialize();
    char a = 'x';
    while (a != 'z') {
        tutorial();
        std::cin >> a;
    }
    Module::Shutdown();
    return 0;
}